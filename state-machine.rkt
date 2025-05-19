#lang racket/base

(require racket/contract)
(require racket/generator)
(require racket/list)
(require racket/match)
(require net/ip)
(require "logger.rkt")
(require "message.rkt")
(require (prefix-in retry: "retry.rkt"))
(require "retry.rkt")

(provide make-state-machine
         (struct-out send-msg)
         (struct-out iface-bind)
         (struct-out iface-unbind)
         (struct-out incoming)
         (struct-out lease-info)
         (struct-out time-event))

; fields are ip-address?
(struct lease-info (client-addr server-addr) #:transparent)

(struct lease-instants (expiry renewal rebinding) #:transparent)

; outgoing event structs. to can be 'broadcast or ip-address?
(struct send-msg (msg to) #:transparent)

(struct iface-bind (info) #:transparent)
(struct iface-unbind () #:transparent)

; incoming event structs
; incoming needs a time, so the machine knows when packets came in
(struct incoming (now sender msg) #:transparent)
(struct time-event (now) #:transparent)

; base struct for all postal exceptions.
(struct exn:postal exn:fail () #:transparent)

; generator based attempt
(define xid (make-parameter 0))
(define (next-xid!)
  (let ([ret (xid)])
    (xid (add1 (xid)))
    ret))

; this provides a generator context, within which the various state functions below can run.
; for the purposes of testing, the machine can be created directly in a certain state.
; state functions use yield wherever required, forming a "giant" generator. This demonstrates how racket generator (aka continuations) are stackful.
; they will often take the first incoming event as a curried argument.
(define (make-state-machine #:xid [start-xid 0] #:start-state [start-state (init-state)])
  ; should return a generator
  (generator
   (event)
   (parameterize ([xid start-xid])
     ; assume in initial state, although later we want to support starting from rebooting etc.
     ; could do that by accept formals in the generator
     ; need to figure out a better way to enter the right initial state.
     (start-state event))))

; the init state expects a time event to get itself going, because it is one of few states where a transition happens without any outside interaction.
; this is to have a similar contract to the other states, all of which expect to make progess by timer or incoming messages.
(define/match ((init-state) event)
  [((time-event now)) (to-selecting-state now)])

(define (to-selecting-state now)
  (define timeout-instant (+ 10000 now))
  (define xid (next-xid!))
  ((selecting-state timeout-instant xid)
   (yield timeout-instant (list (send-msg (make-dhcpdiscover xid) 'broadcast)))))

(define (make-dhcpdiscover xid)
  (message 'discover
           xid
           0
           (number->ipv4-address 0)
           (number->ipv4-address 0)
           (number->ipv4-address 0)
           (number->ipv4-address 0)
           null))

(define ((selecting-state timeout expected-xid) first-event)
  (define/match (reasonable-offer? incom)
    [((incoming _ src (struct* message ([type 'offer] [xid (== expected-xid)])))) #t]
    [(_) #f])

  (define (maybe-offer-list incom)
    (if (reasonable-offer? incom)
        (begin
          (log-postal-debug "considered reasonable ~a" incom)
          (list incom))
        (begin
          (log-postal-debug "not reasonable")
          null)))

  (let loop ([offers null]
             [incoming-event first-event])
    (log-postal-debug "entered selecting loop with offers ~a and event ~a~n" offers incoming-event)
    ; one of the tiny edge cases here is that the outgoing DHCPDISCOVER may not actually get
    ; sent right away, in which case the timeout is not allowing a full 10 seconds for discovery.
    (match incoming-event
      [(time-event now)
       (if (empty? offers)
           ; TODO: Client should probably retry, and this needs to indicate that somehow.
           (error "Need to handle not getting any offers")
           (to-requesting-state now (first offers)))]

      [(and (incoming _ _ _) incom)
       (log-postal-debug "incoming ~a~n" incom)
       ; accumulate offers, nothing to send.
       (loop (append offers (maybe-offer-list incom)) (yield timeout null))])))

(struct exn:postal:invalid-time exn:postal () #:transparent)

(define (raise-invalid-time str msg dur)
  (raise (exn:postal:invalid-time (format "~a~n  time: ~a~n  message type: ~a~n  message options: ~a"
                                          str
                                          dur
                                          (message-type msg)
                                          (message-options msg))
                                  (current-continuation-marks))))

(define (make-request offer)
  (list (send-msg (request-from-offer (next-xid!) offer) 'broadcast)))

(define (to-requesting-state when offer)
  (define rpolicy (retry-policy 4 4000 2000))
  (define rstate (start-retry rpolicy when))
  (define event (yield (get-deadline rstate) (make-request offer)))
  ((requesting-state rstate when offer) event))

; TODO: Propagate the current now via requesting to bound, since t1 and t2 are calculated from there
; TODO: Save the xid in the requesting-state so that the ack can be matched
(define (jitter)
  ; move from positive to split around 0
  (let ([j 2000]) (- (quotient j 2) (random j))))

(define ((requesting-state rstate now offer) event)
  (let loop ([state rstate]
             [last-attempt-time now]
             [the-event event])
    (match the-event
      [(time-event now)
       #:when (expired? state now)
       (define next-state (next-retry state now))
       (if next-state
           (begin
             (loop next-state now (yield (get-deadline next-state) (make-request offer))))
           (begin
             (log-postal-debug "all attempts done. next sm invocation will be passed to init-state")
             ((init-state) (yield now null))))]
      [(time-event now) (loop state (yield (get-deadline state) null))]
      [(and (incoming _ _ _) incom)
       (with-handlers ([exn:postal:invalid-time?
                        (lambda (e)
                          (log-postal-warning "Invalid DHCPACK ignored: ~e" e)
                          ; treat this as if we never got a message.
                          ; this means we just keep waiting for the original timeout to elapse.
                          (loop state last-attempt-time (yield (get-deadline state) null)))])
         (maybe-ack-to-bound incom last-attempt-time))])))

(define/match (maybe-ack-to-bound incom base-instant)
  ; TODO: Handle other kinds of messages instead of assuming this is an ack
  ; TODO: Match with chosen
  ; TODO: If renewal time or rebinding time are not specified, they should be 50% and 87.5% of the lease time with some jitter.
  ; TODO: Store lease time.
  [((incoming in-time src msg) _)
   (let* ([lease-opt (optionsf msg 'lease-time)]
          ; the wierd and trick is to make sure, if lease-opt is #f then the expression is immediately #f.
          ; Otherwise the comparison will encounter a contract failure.
          ; If the comparison succeeds, the value of the expression should be lease-opt, and not #t.
          [lease-dur (or (and lease-opt (> lease-opt 5) lease-opt)
                         (raise-invalid-time "lease time not found, or too small" msg lease-opt))]
          [renew-dur (or (optionsf msg 'renewal-time) (quotient lease-dur 2))]
          [rebind-dur (or (optionsf msg 'rebinding-time) (truncate (* 0.875 lease-dur)))])
     ; basic validation.
     (when (> 1 renew-dur)
       (raise-invalid-time "renewal time too small" msg renew-dur))
     (when (> 2 rebind-dur)
       (raise-invalid-time "rebinding time too small" msg rebind-dur))

     ; TODO: Handle 'lease-time and store it!

     (let ([info (lease-info-from-ack msg)]
           [instants (lease-instants (+ base-instant (seconds->milliseconds lease-dur))
                                     (+ base-instant (seconds->milliseconds renew-dur))
                                     (+ base-instant (seconds->milliseconds rebind-dur)))])
       (to-bound-state info instants)))])

(define (to-bound-state info instants)
  ((bound-state info instants) (yield (lease-instants-renewal instants) (list (iface-bind info)))))

(define ((bound-state info l-instants) first-incom)
  (let loop ([incoming-event first-incom])
    (match incoming-event
      [(time-event now)
       #:when (>= now (lease-instants-renewal l-instants))
       (to-renewing-state info l-instants now)]
      [(time-event now)
       (log-postal-debug "Hanging out in bound until timer expires")
       (loop (yield (lease-instants-renewal l-instants) null))]
      [_ (error "Don't know what to do with a packet when in bound.")])))

;; Helper function to calculate the absolute time when the next request should be sent
;; Takes current time and rebinding time, returns the absolute timeout instant
(define (calculate-request-timeout-instant now rebinding-time)
  ;; Calculate time until rebinding
  (define time-until-rebind (- rebinding-time now))
  ;; Calculate half of the remaining time as per RFC
  (define half-remaining (quotient time-until-rebind 2))
  ;; Ensure minimum interval of 60 seconds
  (define interval (max (* 60 1000) half-remaining))
  ;; Return the absolute timeout instant
  (+ now interval))

; umm, recursion? i.e. renewing-state, rather than being a loop, calls itself with a transition that causes a packet send?
; any way, renewing state needs:
; - lease information
; - current time (because the first entry is from an event handled by the previous state)
; - lease instants information
; when it sends a dhcprequest, it has to remember:
; - the time when it send the request, so that on receiving an ack, that is the basis for the lease calculation. this has to be carried around.
; - the request xid to match acks against. xid re-use is allowed.
; - almost feel like these 2 can be a struct?

(define (to-renewing-state info l-instants now)
  ;; Send initial DHCPREQUEST to the server that provided our lease
  (define request-xid (next-xid!))

  ;; Calculate when the next request should be sent
  (define next-request-time
    (calculate-request-timeout-instant now (lease-instants-rebinding l-instants)))

  ;; Send the initial request and get the first event
  (define first-event
    (yield next-request-time
           (list (send-msg (request-from-lease info request-xid) (lease-info-server-addr info)))))

  ;; Transition to the renewing state with the first event and the time of the last request (now)
  ((renewing-state-new info l-instants now) first-event))

;; New implementation of renewing state with improved validation and structure
(define ((renewing-state-new info l-instants last-request-time) first-event)
  ;; Use the passed-in last-request-time instead of deriving it from the event
  (define request-xid (next-xid!))

  (log-postal-debug "renewing-state-new: Starting with last_request_time=~a, rebinding=~a"
                    last-request-time
                    (lease-instants-rebinding l-instants))

  (let loop ([incoming-event first-event]
             [last-request-time last-request-time])
    ;; Calculate when the next request should be sent based on the last request time
    (define time-for-next-request
      (calculate-request-timeout-instant last-request-time (lease-instants-rebinding l-instants)))

    (log-postal-debug "renewing-state-new: time_for_next_request=~a" time-for-next-request)

    (match incoming-event
      ;; If we reach T2 (rebinding time), transition to rebinding state
      [(time-event now)
       #:when (>= now (lease-instants-rebinding l-instants))
       (log-postal-debug "renewing-state-new: Reached rebinding time, transitioning")
       (to-rebinding-state info l-instants now)]

      ;; Handle periodic retransmission
      [(time-event now)
       #:when (>= now time-for-next-request)

       ;; Time to send another request
       (let ([next-time (calculate-request-timeout-instant now
                                                           (lease-instants-rebinding l-instants))])
         (log-postal-debug "renewing-state-new: Sending request, next wakeup at ~a" next-time)
         (loop (yield next-time
                      (list (send-msg (request-from-lease info request-xid)
                                      (lease-info-server-addr info))))
               now))]
      [(time-event now)
       ;; Not time yet, just wait
       (log-postal-debug "renewing-state-new: Not time yet, waiting until ~a" time-for-next-request)
       (loop (yield time-for-next-request null) last-request-time)]

      ;; Handle DHCPNAK - release address and go back to init
      [(incoming now sender (and msg (struct* message ([type 'nak] [xid (== request-xid)]))))
       ;; Validate the sender is our server
       (log-postal-debug "renewing-state-new: Received NAK from ~a" sender)
       (when (ip-address=? sender (lease-info-server-addr info))
         (log-postal-debug "renewing-state-new: Valid NAK, returning to init state")
         ((init-state) (yield (+ 2000 now) (list (iface-unbind)))))]

      ;; Handle DHCPACK - renew lease and go back to bound
      [(incoming now sender (and msg (struct* message ([type 'ack] [xid (== request-xid)]))))
       ;; Validate the sender is our server and the address matches
       (log-postal-debug "renewing-state-new: Received ACK from ~a" sender)
       (when (and (ip-address=? sender (lease-info-server-addr info))
                  (ip-address=? (message-yiaddr msg) (lease-info-client-addr info)))
         (log-postal-debug "renewing-state-new: Valid ACK, returning to bound state")
         (maybe-ack-to-bound incoming-event last-request-time))]

      ;; Ignore any other messages - use time-for-next-request for consistent scheduling
      [other
       (log-postal-debug "renewing-state-new: Ignoring event ~a, waiting until ~a"
                         other
                         time-for-next-request)
       (loop (yield time-for-next-request null) last-request-time)])))

(define (to-rebinding-state info l-instants request-instant)
  (define request-xid (next-xid!))
  ;; Calculate when the next request should be sent
  (define next-request-time
    (calculate-request-timeout-instant request-instant (lease-instants-expiry l-instants)))
  (define next-evt
    (yield next-request-time (list (send-msg (request-from-lease info request-xid) 'broadcast))))
  (log-postal-debug "next-evt when transitioning to rebinding state ~a" next-evt)
  ((rebinding-state info l-instants request-instant) next-evt))

; TODO: It seems like renewing and rebinding states have the same logic with slight differences. See what can be refactored.
(define ((rebinding-state info l-instants request-instant) first-incom)
  (define request-xid (next-xid!))
  (let loop ([incoming-event first-incom]
             [last-request-time request-instant])
    (define time-for-next-request
      (calculate-request-timeout-instant last-request-time (lease-instants-expiry l-instants)))

    (match incoming-event
      ;; If we exceed the lease time (estimated), unbind and go back to init
      [(time-event now)
       #:when (> now (lease-instants-expiry l-instants))
       ((init-state) (yield (+ 2000 now) (list (iface-unbind))))]

      ;; Handle periodic retransmission
      [(time-event now)
       #:when (>= now time-for-next-request)

       ;; Time to send another request
       (let ([next-time (calculate-request-timeout-instant now
                                                           (lease-instants-rebinding l-instants))])
         (loop (yield next-time (list (send-msg (request-from-lease info request-xid) 'broadcast)))
               now))]
      ;; Not time yet, just wait
      [(time-event now) (loop (yield time-for-next-request null) last-request-time)]

      [(incoming now sender (and msg (struct* message ([type 'nak] [xid (== request-xid)]))))
       ;; No need to validate sender in rebinding - any server can respond
       ((init-state) (yield (+ 2000 now) (list (iface-unbind))))]

      [(incoming _ sender (and msg (struct* message ([type 'ack] [xid (== request-xid)]))))
       ;; Validate the address matches
       (when (ip-address=? (message-yiaddr msg) (lease-info-client-addr info))
         (maybe-ack-to-bound incoming-event request-instant))]

      ;; Ignore any other messages
      [other
       (log-postal-debug "rebinding-state: Ignoring event ~a" other)
       (loop (yield time-for-next-request null) last-request-time)])))

(define (request-from-lease info xid)
  (message 'request
           xid
           0
           (lease-info-client-addr info)
           (number->ipv4-address 0)
           (number->ipv4-address 0)
           (number->ipv4-address 0)
           null))

(define (lease-info-from-ack msg)
  (unless (eq? (message-type msg) 'ack)
    (error "Not a DHCPACK message"))

  (lease-info (message-yiaddr msg) (optionsf msg 'server-identifier)))

(define (seconds->milliseconds sec)
  (* 1000 sec))

(define/match (request-from-offer xid offer)
  [(_ (struct incoming (_ sender msg)))
   (message 'request
            xid
            ; TODO: secs, should be the same as discover
            0
            (number->ipv4-address 0)
            (number->ipv4-address 0)
            (number->ipv4-address 0)
            (number->ipv4-address 0)
            (list (message-option 'requested-ip-address (message-yiaddr msg))
                  (message-option 'server-identifier (optionsf msg 'server-identifier))))])

(module+ test
  (require rackunit)
  (require net/ip)

  (define k-test-renewal-time 1800)
  (define k-test-rebinding-time 3150)
  (define k-test-lease-time 3600)

  (define canonical-server-ip (make-ip-address "192.168.11.1"))

  (define (extract-option options tag)
    (findf (lambda (opt) (equal? (message-option-tag opt) tag)) options))

  (define (make-incoming now msg [sender canonical-server-ip])
    (incoming now sender msg))

  ; expiry is required. if renewal/rebinding are not passed, they are derived using the RFC scaling constants.
  ; hmm, scaling would require needing to know "now", since the math is applied from the "duration of lease".
  ; assumed to be 0.
  (define (make-lease-instants #:expiry expiry
                               #:renewal [renewal #f]
                               #:rebinding [rebinding #f]
                               #:now [now 0])
    (lease-instants expiry
                    (or renewal (* 0.5 (- expiry now)))
                    (or rebinding (* 0.875 (- expiry now)))))

  (test-case "Starting in init leads to a request to send DHCPDISCOVER"
    (define s (make-state-machine))
    ; no input required
    (define-values (_ events) (s (time-event 0)))
    (check-match events (list (send-msg (struct* message ([type 'discover])) 'broadcast))))

  ; will need to figure out how to test intermediate states
  ; while getting the machine to that state.
  (test-case "When in selecting, offers are considered for 10 seconds"
    (define s (make-state-machine #:xid 34 #:start-state (selecting-state 0 34)))
    (let-values ([(_ events) (s (make-incoming 8000
                                               (message 'offer
                                                        34
                                                        0
                                                        (number->ipv4-address 0)
                                                        (make-ip-address "192.168.11.3")
                                                        (number->ipv4-address 0)
                                                        (number->ipv4-address 0)
                                                        (list (message-option
                                                               'server-identifier
                                                               canonical-server-ip)))))])
      (check-equal? events null))

    (let-values ([(_ events) (s (make-incoming
                                 9900
                                 (message 'offer
                                          34
                                          0
                                          (number->ipv4-address 0)
                                          (make-ip-address "192.168.11.6")
                                          (number->ipv4-address 0)
                                          (number->ipv4-address 0)
                                          (list (message-option 'server-identifier
                                                                (make-ip-address "192.168.11.99"))))
                                 (make-ip-address "192.168.11.99")))])
      (check-equal? events null))

    (let-values ([(_ events) (s (time-event 11000))])
      (check-match
       events
       (list (send-msg (and msg (struct* message ([type 'request] [options opts]))) 'broadcast))
       (and (equal? (optionsf msg 'server-identifier) canonical-server-ip)
            (equal? (optionsf msg 'requested-ip-address) (make-ip-address "192.168.11.3"))))))

  (test-case "An offer with a non-matching xid is ignored."
    (define s (make-state-machine #:xid 34 #:start-state (selecting-state 0 34)))
    (s (make-incoming 4000
                      (message 'offer
                               72
                               0
                               (number->ipv4-address 0)
                               (number->ipv4-address 0)
                               (number->ipv4-address 0)
                               (number->ipv4-address 0)
                               null)))
    ; TODO: Once the machine handles this a bit gracefully, fix this.
    (check-exn exn:fail? (lambda () (s (time-event 11000)))))

  (test-case "Entering the BOUND state issues an interface binding event"
    (define s
      (make-state-machine
       #:xid 42
       #:start-state
       (requesting-state (start-retry (retry-policy 4 4000 2000) 11100)
                         11100
                         (make-incoming 9500
                                        (message 'offer
                                                 34
                                                 0
                                                 (number->ipv4-address 0)
                                                 (make-ip-address "192.168.11.12")
                                                 (number->ipv4-address 0)
                                                 (number->ipv4-address 0)
                                                 (list (message-option 'server-identifier
                                                                       canonical-server-ip)))))))
    (define-values (_ outgoing-events)
      (s (make-incoming 11100
                        (message 'ack
                                 42
                                 0
                                 (make-ip-address "192.168.11.12")
                                 (make-ip-address "192.168.11.12")
                                 (number->ipv4-address 0)
                                 (number->ipv4-address 0)
                                 (list (message-option 'renewal-time 1800)
                                       (message-option 'rebinding-time 3150)
                                       (message-option 'lease-time 3600)
                                       (message-option 'server-identifier canonical-server-ip))))))

    (check-match outgoing-events
                 (list (iface-bind (lease-info (== (make-ip-address "192.168.11.12"))
                                               (== (make-ip-address "192.168.11.1")))))))

  (test-case "An ACK with no lease time is ignored. Eventually moves back to init."
    ; one DHCPREQUEST will be sent entering this state, and is not part of this test.
    (define s
      (make-state-machine
       #:xid 42
       #:start-state
       (requesting-state (start-retry (retry-policy 4 4000 2000) 130)
                         130
                         (make-incoming 100
                                        (message 'offer
                                                 34
                                                 0
                                                 (number->ipv4-address 0)
                                                 (make-ip-address "192.168.11.12")
                                                 (number->ipv4-address 0)
                                                 (number->ipv4-address 0)
                                                 (list (message-option 'server-identifier
                                                                       canonical-server-ip)))))))

    ; this is treated as malformed, so ignored and machine continues to wait for timeout.
    (define-values (wakeup-at outgoing-events)
      (s (make-incoming 1100
                        (message 'ack
                                 42
                                 0
                                 (make-ip-address "192.168.11.12")
                                 (make-ip-address "192.168.11.12")
                                 (number->ipv4-address 0)
                                 (number->ipv4-address 0)
                                 (list (message-option 'renewal-time 1800)
                                       (message-option 'rebinding-time 3150)
                                       (message-option 'server-identifier canonical-server-ip))))))

    ; there are 3 retransmits of the DHCPREQUEST for the original offer.
    (define final-wakeup
      (for/fold ([next-wakeup wakeup-at]) ([i (in-range 3)])
        (match-let-values ([(wakeup-at outgoing-events) (s (time-event next-wakeup))])
          (check-match outgoing-events
                       (list (struct* send-msg
                                      ([to 'broadcast] [msg (struct* message ([type 'request]))]))))
          wakeup-at)))

    (match-let-values ([(wakeup-at outgoing-events) (s (time-event final-wakeup))])
      (check-pred null? outgoing-events "expected no outgoing events after all attempts exhausted")

      ; init-state should trigger sending a discover on the transition to selecting.
      (match-let-values ([(wakeup-at outgoing-events) (s (time-event wakeup-at))])
        (check-match outgoing-events
                     (list (send-msg (struct* message ([type 'discover])) 'broadcast))))))
  (test-case "An ACK with no renewal time picks a reasonable default"
    (define s
      (make-state-machine
       #:xid 42
       #:start-state
       (requesting-state (start-retry (retry-policy 4 4000 2000) 11100)
                         11100
                         (make-incoming 9500
                                        (message 'offer
                                                 34
                                                 0
                                                 (number->ipv4-address 0)
                                                 (make-ip-address "192.168.11.12")
                                                 (number->ipv4-address 0)
                                                 (number->ipv4-address 0)
                                                 (list (message-option 'server-identifier
                                                                       canonical-server-ip)))))))
    ; Send an ACK without renewal-time option
    (define-values (wakeup-at outgoing-events)
      (s (make-incoming 11100
                        (message 'ack
                                 42
                                 0
                                 (make-ip-address "192.168.11.12")
                                 (make-ip-address "192.168.11.12")
                                 (number->ipv4-address 0)
                                 (number->ipv4-address 0)
                                 (list (message-option 'rebinding-time k-test-rebinding-time)
                                       (message-option 'lease-time k-test-lease-time)
                                       (message-option 'server-identifier canonical-server-ip))))))

    ; Verify we got a bind event
    (check-match outgoing-events
                 (list (iface-bind (lease-info (== (make-ip-address "192.168.11.12"))
                                               (== canonical-server-ip)))))

    ; Verify the next wakeup is at 50% of lease time (1800s = 1800000ms)
    (check-equal? wakeup-at (+ 11100 (* 1000 (quotient k-test-lease-time 2)))))

  (test-case "Once t1 expires, a (unicast) DHCPREQUEST is sent"
    (define renewal-ms (seconds->milliseconds k-test-renewal-time))
    (define s
      (make-state-machine
       #:xid 42
       #:start-state
       (bound-state (lease-info (make-ip-address "192.168.11.12") (make-ip-address "192.168.11.1"))
                    (lease-instants (seconds->milliseconds k-test-lease-time)
                                    renewal-ms
                                    (seconds->milliseconds k-test-rebinding-time)))))
    ; the setup step made the transition to bound at 11100.
    ; no events until the renewal time.
    (for ([time-delta-ms (in-range 0 renewal-ms (seconds->milliseconds 100))])
      (let-values ([(_ events) (s (time-event (+ 11100 time-delta-ms)))])
        (check-true (empty? events))))
    ; now, renew.
    (let-values ([(_ events) (s (time-event (+ 11100 renewal-ms)))])
      (check-match events
                   (list (send-msg (struct* message ([type 'request])) (== canonical-server-ip))))))

  (test-case "When in renewing, if enough time has elapsed, rebinding is triggered"
    (define s
      (make-state-machine #:xid 42
                          #:start-state
                          (renewing-state-new (lease-info (make-ip-address "192.168.11.12")
                                                          (make-ip-address "192.168.11.1"))
                                              (lease-instants (truncate (/ 3000 0.875)) #f 3000)
                                              1000)))
    (define-values (_ outgoing-events) (s (time-event 3200)))
    (check-match outgoing-events (list (send-msg (struct* message ([type 'request])) 'broadcast))))

  (test-case "When in renewing, renewal is re-attempted periodically"
    (define s
      (make-state-machine
       #:xid 42
       #:start-state (renewing-state-new
                      (lease-info (make-ip-address "192.168.11.12") (make-ip-address "192.168.11.1"))
                      (lease-instants (truncate (/ (* 30 60 1000) 0.875)) #f (* 30 60 1000))
                      1000)))

    #|
   In both RENEWING and REBINDING states, if the client receives no
   response to its DHCPREQUEST message, the client SHOULD wait one-half
   of the remaining time until T2 (in RENEWING state) and one-half of
   the remaining lease time (in REBINDING state), down to a minimum of
   60 seconds, before retransmitting the DHCPREQUEST message.

   With the state created as if the last DHCPREQUEST was sent at 1000, and the rebinding time is at instant 1800000.
   1. The first "receives no response" timeout will be at 1000 + (1800000 - 1000) / 2 = 900500, when a new request will be sent.
   2. If no response is received to that one by 900500 + (1800000 - 900500) / 2 = 1350250, when a new request will be sent.
   3. 1350250 + (1800000 - 1350250) / 2 = 1575125,
   4. 1687562
   5. 1743781
   6. 1771890
   Now (1800000 - 1771890) = 28110 which is < 60000, so start capping at no more than once every 60000.
   7. 1831890 which exceeds the rebinding time, so exit the state.
   |#

    ; First attempt already sent on state entry. Now, nothing should happen until the retry time.
    (define-values (wakeup-at outgoing-events) (s (time-event 61000)))
    (check-equal? wakeup-at 900500)
    (check-equal? outgoing-events null)

    ; expect retries if we keep waking up the machine at the next time it indicates, stopping when the next time is > the rebinding instant.
    (define rebinding-wakeup-at
      (for/fold ([wakeup-at 900500]) ([k (in-naturals)])
        #:break (>= wakeup-at 1800000)
        (define-values (nw events) (s (time-event wakeup-at)))
        (check-match events
                     (list (send-msg (struct* message ([type 'request])) (== canonical-server-ip))))
        nw))

    (define-values (_ rb-events) (s (time-event (+ 5 (* 30 60 1000)))))
    (check-match rb-events (list (send-msg (struct* message ([type 'request])) 'broadcast))))

  (test-case "When in renewing, unexpected packets are ignored"
    (define s
      (make-state-machine #:xid 42
                          #:start-state
                          (renewing-state-new (lease-info (make-ip-address "192.168.11.12")
                                                          (make-ip-address "192.168.11.1"))
                                              (lease-instants #f #f 3000)
                                              1000)))
    ; Send an offer - should be ignored
    (define-values (wakeup1 events1)
      (s (make-incoming 1500
                        canonical-server-ip
                        (message 'offer
                                 99
                                 0
                                 (number->ipv4-address 0)
                                 (make-ip-address "192.168.11.12")
                                 (number->ipv4-address 0)
                                 (number->ipv4-address 0)
                                 (list (message-option 'server-identifier canonical-server-ip))))))
    (check-equal? events1 null)

    ; Send a discover - should be ignored
    (define-values (wakeup2 events2)
      (s (make-incoming 1600
                        canonical-server-ip
                        (message 'discover
                                 99
                                 0
                                 (number->ipv4-address 0)
                                 (number->ipv4-address 0)
                                 (number->ipv4-address 0)
                                 (number->ipv4-address 0)
                                 null))))
    (check-equal? events2 null)

    ; Send an ACK with wrong XID - should be ignored
    (define-values (wakeup3 events3)
      (s (make-incoming 1700
                        canonical-server-ip
                        (message 'ack
                                 99
                                 0
                                 (make-ip-address "192.168.11.12")
                                 (make-ip-address "192.168.11.12")
                                 (number->ipv4-address 0)
                                 (number->ipv4-address 0)
                                 (list (message-option 'renewal-time k-test-renewal-time)
                                       (message-option 'rebinding-time k-test-rebinding-time)
                                       (message-option 'lease-time k-test-lease-time)
                                       (message-option 'server-identifier canonical-server-ip))))))
    (check-equal? events3 null))

  (test-case "When in renewing, a nak leads to reset"
    (define s
      (make-state-machine #:xid 42
                          #:start-state
                          (renewing-state-new (lease-info (make-ip-address "192.168.11.12")
                                                          (make-ip-address "192.168.11.1"))
                                              (lease-instants (truncate (/ 3000 0.875)) #f 3000)
                                              1000)))
    (define-values (_ outgoing-events)
      (s (incoming 2000
                   canonical-server-ip
                   (message 'nak
                            42
                            1432
                            (number->ipv4-address 0)
                            (number->ipv4-address 0)
                            (number->ipv4-address 0)
                            (number->ipv4-address 0)
                            (list (message-option 'server-identifier canonical-server-ip))))))
    (check-match outgoing-events (list (iface-unbind))))

  (test-case "When in renewing, an ack moves back to bound"
    (define s
      (make-state-machine #:xid 42
                          #:start-state
                          (renewing-state-new (lease-info (make-ip-address "192.168.11.12")
                                                          (make-ip-address "192.168.11.1"))
                                              (lease-instants (truncate (/ 3000 0.875)) #f 3000)
                                              1000)))

    (define-values (_ outgoing-events)
      (s (incoming 2000
                   canonical-server-ip
                   (message 'ack
                            42
                            1432
                            (make-ip-address "192.168.11.12")
                            (make-ip-address "192.168.11.12")
                            (number->ipv4-address 0)
                            (number->ipv4-address 0)
                            (list (message-option 'renewal-time 17000)
                                  (message-option 'rebinding-time 34500)
                                  (message-option 'lease-time 3600)
                                  (message-option 'server-identifier canonical-server-ip))))))
    (check-match outgoing-events
                 (list (iface-bind (lease-info (== (make-ip-address "192.168.11.12"))
                                               (== (make-ip-address "192.168.11.1")))))))

  (test-case "When in rebinding, rebinding is re-attempted periodically"
    (define s
      (make-state-machine #:xid 42
                          #:start-state
                          (rebinding-state (lease-info (make-ip-address "192.168.11.12")
                                                       (make-ip-address "192.168.11.1"))
                                           (lease-instants (* 35 60 1000) #f (* 30 60 1000))
                                           1000)))
    ; First attempt already sent on state entry
    ; Check that after half the remaining time (500ms), another attempt is made
    (define-values (wakeup-at outgoing-events) (s (time-event 1500)))
    (check-match outgoing-events (list (send-msg (struct* message ([type 'request])) 'broadcast)))

    ; Verify minimum interval of 60 seconds is respected
    (define-values (next-wakeup next-events) (s (time-event (+ wakeup-at 60000))))
    (check-match next-events (list (send-msg (struct* message ([type 'request])) 'broadcast))))

  (test-case "When in rebinding, unexpected packets are ignored"
    (define s
      (make-state-machine #:xid 42
                          #:start-state (rebinding-state (lease-info (make-ip-address "192.168.11.12")
                                                                     (make-ip-address "192.168.11.1"))
                                                         1000
                                                         42)))
    ; Send an offer - should be ignored
    (define-values (wakeup1 events1)
      (s (make-incoming 1100
                        canonical-server-ip
                        (message 'offer
                                 99
                                 0
                                 (number->ipv4-address 0)
                                 (make-ip-address "192.168.11.12")
                                 (number->ipv4-address 0)
                                 (number->ipv4-address 0)
                                 (list (message-option 'server-identifier canonical-server-ip))))))
    (check-equal? events1 null)

    ; Send a discover - should be ignored
    (define-values (wakeup2 events2)
      (s (make-incoming 1200
                        canonical-server-ip
                        (message 'discover
                                 99
                                 0
                                 (number->ipv4-address 0)
                                 (number->ipv4-address 0)
                                 (number->ipv4-address 0)
                                 (number->ipv4-address 0)
                                 null))))
    (check-equal? events2 null)

    ; Send an ACK with wrong XID - should be ignored
    (define-values (wakeup3 events3)
      (s (make-incoming 1300
                        canonical-server-ip
                        (message 'ack
                                 99
                                 0
                                 (make-ip-address "192.168.11.12")
                                 (make-ip-address "192.168.11.12")
                                 (number->ipv4-address 0)
                                 (number->ipv4-address 0)
                                 (list (message-option 'renewal-time k-test-renewal-time)
                                       (message-option 'rebinding-time k-test-rebinding-time)
                                       (message-option 'lease-time k-test-lease-time)
                                       (message-option 'server-identifier canonical-server-ip))))))
    (check-equal? events3 null))

  (test-case "When in rebinding, a nak leads to reset"
    (define s
      (make-state-machine #:xid 42
                          #:start-state (rebinding-state (lease-info (make-ip-address "192.168.11.12")
                                                                     (make-ip-address "192.168.11.1"))
                                                         1000
                                                         42)))
    (define-values (_ outgoing-events)
      (s (incoming 1100
                   canonical-server-ip
                   (message 'nak
                            42
                            1432
                            (number->ipv4-address 0)
                            (number->ipv4-address 0)
                            (number->ipv4-address 0)
                            (number->ipv4-address 0)
                            (list (message-option 'server-identifier canonical-server-ip))))))
    (check-match outgoing-events (list (iface-unbind))))

  (test-case "When in rebinding, an ack moves back to bound"
    (define s
      (make-state-machine #:xid 42
                          #:start-state (rebinding-state (lease-info (make-ip-address "192.168.11.12")
                                                                     (make-ip-address "192.168.11.1"))
                                                         1000
                                                         42)))

    (define-values (_ outgoing-events)
      (s (incoming 1100
                   canonical-server-ip
                   (message 'ack
                            42
                            1432
                            (make-ip-address "192.168.11.12")
                            (make-ip-address "192.168.11.12")
                            (number->ipv4-address 0)
                            (number->ipv4-address 0)
                            (list (message-option 'renewal-time 17000)
                                  (message-option 'rebinding-time 34500)
                                  (message-option 'lease-time 3600)
                                  (message-option 'server-identifier canonical-server-ip))))))
    (check-match outgoing-events
                 (list (iface-bind (lease-info (== (make-ip-address "192.168.11.12"))
                                               (== (make-ip-address "192.168.11.1")))))))

  #;(test-case "DHCPOFFER/ACK/NACK are discarded in BOUND state"
      (fail "TODO"))

  #;(test-case "Ensure all states handle an eventual timeout"
      ))
