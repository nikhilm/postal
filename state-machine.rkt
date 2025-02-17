#lang racket/base

(require racket/contract)
(require racket/generator)
(require racket/list)
(require racket/match)
(require net/ip)
(require "logger.rkt")
(require "message.rkt")
(require (prefix-in retry: "retry.rkt"))
(require "retry-ai.rkt")

(provide make-state-machine
         (struct-out send-msg)
         (struct-out iface-bind)
         (struct-out iface-unbind)
         (struct-out incoming)
         (struct-out lease-info)
         (struct-out time-event))

; fields are ip-address?
(struct lease-info (client-addr server-addr) #:transparent)

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
  (generator (event)
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
  ((selecting-state timeout-instant xid) (yield timeout-instant (list (send-msg (make-dhcpdiscover xid) 'broadcast)))))

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
        (begin (log-postal-debug "considered reasonable ~a" incom)
               (list incom))
        (begin (log-postal-debug "not reasonable") null)))

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
  (raise
   (exn:postal:invalid-time
    (format "~a~n  time: ~a~n  message type: ~a~n  message options: ~a" str dur (message-type msg) (message-options msg))
    (current-continuation-marks))))

(define (make-request offer)
  (list (send-msg (request-from-offer (next-xid!) offer) 'broadcast)))

(define (to-requesting-state when offer)
  (define event (yield (+ 4000 when)
                       (make-request offer)))
  ((requesting-state when offer) event))


; TODO: Propagate the current now via requesting to bound, since t1 and t2 are calculated from there
; TODO: Save the xid in the requesting-state so that the ack can be matched
(define (jitter)
  ; move from positive to split around 0
  (let ([j 2000])
    (- (quotient j 2) (random j))))

(require "retry-ai.rkt")

(define ((requesting-state now offer) event)
  (define rpolicy (retry-policy 4 4000 2000))
  (define rstate (start-retry rpolicy now))

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
                         (raise-invalid-time  "lease time not found, or too small" msg lease-opt))]
          [renew-dur (or (optionsf msg 'renewal-time) (quotient lease-dur 2))]
          [rebind-dur (or (optionsf msg 'rebinding-time) (truncate (* 0.875 lease-dur)))])
     ; basic validation.
     (when (> 1 renew-dur)
       (raise-invalid-time "renewal time too small" msg renew-dur))
     (when (> 2 rebind-dur)
       (raise-invalid-time "rebinding time too small" msg rebind-dur))

     ; TODO: Handle 'lease-time and store it!

     (let ([info (lease-info-from-ack msg)]
           [renew-instant (+ base-instant (seconds->milliseconds renew-dur))]
           [rebind-instant (+ base-instant (seconds->milliseconds rebind-dur))])
       (to-bound-state info renew-instant rebind-instant)))])

(define (to-bound-state info renew-instant rebind-instant)
  ((bound-state info renew-instant rebind-instant) (yield renew-instant (list (iface-bind info)))))

(define ((bound-state info renew-instant rebind-instant) first-incom)
  (let loop ([incoming-event first-incom])
    (match incoming-event
      [(time-event now)
       (cond
         [(> now renew-instant)
          (to-renewing-state info now rebind-instant)]
         [else (log-postal-debug "Hanging out in bound until timer expires") (loop (yield (+ 5000 now) null))])]
      [_ (error "Don't know what to do with a packet when in bound.")])))

(define (to-renewing-state info request-instant rebind-instant)
  ; TODO: Should actually wait "one half of the remaining time until T2 [...] down to a minimum of 60 seconds,
  ; before retransmitting the DHCP request.".
  (define request-xid (next-xid!))
  (define next-evt (yield rebind-instant (list (send-msg (request-from-lease info request-xid) (lease-info-server-addr info)))))
  (log-postal-debug "next-evt when transitioning to renewing state ~a" next-evt)
  ((renewing-state info rebind-instant request-instant request-xid) next-evt))

(define ((renewing-state info rebind-instant request-instant request-xid) first-incom)
  (let loop ([incoming-event first-incom])
    (match incoming-event
      [(time-event now) #:when (> now rebind-instant)
                        (to-rebinding-state info now)]
      [(time-event now)
       ; TODO: Handle occasionally sending out DHCPREQUEST.
       (loop (yield rebind-instant null))]
      [(incoming now sender (and msg (struct* message ([type 'nak] [xid (== request-xid)]))))
       ; TODO Perform additional validation on the sender.
       ((init-state) (yield (+ 2000 now) (list (iface-unbind))))]
      [(incoming _ sender (and msg (struct* message ([type 'ack] [xid (== request-xid)]))))
       ; TODO Perform additional validation on the sender. plus yiaddr should match the original ciaddr.
       ; TODO: There is a flaw here. Using maybe-ack-to-bound results in an iface-bind event to the caller.
       ; However, since this is a renewal, the client is already bound.
       (maybe-ack-to-bound incoming-event request-instant)]
      [other (log-postal-debug "renaming-state: Ignoring event ~a" other)])))

(define (to-rebinding-state info request-instant)
  (define request-xid (next-xid!))
  ; TODO: Calibrate next wakeup to be based on the RFC calculations.
  (define next-evt (yield (+ 10000 request-instant) (list (send-msg (request-from-lease info request-xid) 'broadcast))))
  (log-postal-debug "next-evt when transitioning to rebinding state ~a" next-evt)
  ((rebinding-state info request-instant request-xid) next-evt))

; TODO: It seems like renewing and rebinding states have the same logic with slight differences. See what can be refactored.
(define ((rebinding-state info request-instant request-xid) first-incom)
  (let loop ([incoming-event first-incom])
    (match incoming-event
      ; TODO: Handle the case where if the time exceeds the lease time, this should unbind and go back to init.
      [(time-event now) #:when (> now (+ 10000 request-instant)) (error "TODO Handle retries and timeouts")]
      [(time-event now) (error "Handle retries and timeouts")]
      [(incoming now sender (and msg (struct* message ([type 'nak] [xid (== request-xid)]))))
       ; TODO Perform additional validation on the sender.
       ((init-state) (yield (+ 2000 now) (list (iface-unbind))))]
      [(incoming _ sender (and msg (struct* message ([type 'ack] [xid (== request-xid)]))))
       ; TODO Perform additional validation on the sender. plus yiaddr should match the original ciaddr.
       ; TODO: There is a flaw here. Using maybe-ack-to-bound results in an iface-bind event to the caller.
       ; However, since this is a renewal, the client is already bound.
       (maybe-ack-to-bound incoming-event request-instant)])))

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

  (lease-info (message-yiaddr msg)
              (optionsf msg 'server-identifier)))

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

  (define canonical-server-ip
    (make-ip-address "192.168.11.1"))

  (define (extract-option options tag)
    (findf (lambda (opt)
             (equal? (message-option-tag opt) tag))
           options))

  (define (make-incoming now msg [sender canonical-server-ip])
    (incoming now sender msg))

  (test-case
   "Starting in init leads to a request to send DHCPDISCOVER"
   (define s (make-state-machine))
   ; no input required
   (define-values (_ events) (s (time-event 0)))
   (check-match events
                (list (send-msg (struct* message ([type 'discover])) 'broadcast))))

  ; will need to figure out how to test intermediate states
  ; while getting the machine to that state.
  (test-case
   "When in selecting, offers are considered for 10 seconds"
   (define s (make-state-machine #:xid 34 #:start-state (selecting-state 0 34)))
   (let-values ([(_ events) (s (make-incoming 8000 (message 'offer
                                                            34
                                                            0
                                                            (number->ipv4-address 0)
                                                            (make-ip-address "192.168.11.3")
                                                            (number->ipv4-address 0)
                                                            (number->ipv4-address 0)
                                                            (list
                                                             (message-option 'server-identifier canonical-server-ip)))))])
     (check-equal? events null))

   (let-values ([(_ events) (s (make-incoming 9900 (message 'offer
                                                            34
                                                            0
                                                            (number->ipv4-address 0)
                                                            (make-ip-address "192.168.11.6")
                                                            (number->ipv4-address 0)
                                                            (number->ipv4-address 0)
                                                            (list (message-option 'server-identifier (make-ip-address "192.168.11.99"))))
                                              (make-ip-address "192.168.11.99")))])
     (check-equal? events null))

   (let-values ([(_ events) (s (time-event 11000))])
     (check-match events
                  (list (send-msg (and msg (struct* message ([type 'request] [options opts]))) 'broadcast))
                  (and (equal? (optionsf msg 'server-identifier)
                               canonical-server-ip)
                       (equal? (optionsf msg 'requested-ip-address)
                               (make-ip-address "192.168.11.3"))))))

  (test-case
   "An offer with a non-matching xid is ignored."
   (define s (make-state-machine #:xid 34 #:start-state (selecting-state 0 34)))
   (s (make-incoming 4000 (message 'offer
                                   72
                                   0
                                   (number->ipv4-address 0)
                                   (number->ipv4-address 0)
                                   (number->ipv4-address 0)
                                   (number->ipv4-address 0)
                                   null)))
   ; TODO: Once the machine handles this a bit gracefully, fix this.
   (check-exn exn:fail? (lambda () (s (time-event 11000)))))

  (test-case
   "Entering the BOUND state issues an interface binding event"
   (define s (make-state-machine #:xid 42
                                 #:start-state (requesting-state
                                                11100
                                                (make-incoming 9500 (message 'offer
                                                                             34
                                                                             0
                                                                             (number->ipv4-address 0)
                                                                             (make-ip-address "192.168.11.12")
                                                                             (number->ipv4-address 0)
                                                                             (number->ipv4-address 0)
                                                                             (list
                                                                              (message-option 'server-identifier canonical-server-ip)))))))
   (define-values (_ outgoing-events)
     (s (make-incoming 11100 (message 'ack
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
                (list
                 (iface-bind
                  (lease-info
                   (== (make-ip-address "192.168.11.12"))
                   (== (make-ip-address "192.168.11.1")))))))

  (test-case
   "An ACK with no lease time is ignored. Eventually moves back to init."
   ; one DHCPREQUEST will be sent entering this state, and is not part of this test.
   (define s (make-state-machine #:xid 42
                                 #:start-state (requesting-state
                                                130
                                                (make-incoming 100 (message 'offer
                                                                            34
                                                                            0
                                                                            (number->ipv4-address 0)
                                                                            (make-ip-address "192.168.11.12")
                                                                            (number->ipv4-address 0)
                                                                            (number->ipv4-address 0)
                                                                            (list
                                                                             (message-option 'server-identifier canonical-server-ip)))))))

   ; this is treated as malformed, so ignored and machine continues to wait for timeout.
   (define-values (wakeup-at outgoing-events)
     (s (make-incoming 1100 (message 'ack
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
     (for/fold ([next-wakeup wakeup-at])
               ([i (in-range 3)])
       (match-let-values ([(wakeup-at outgoing-events) (s (time-event next-wakeup))])
                         (check-match outgoing-events
                                      (list
                                       (struct* send-msg
                                                ([to 'broadcast]
                                                 [msg (struct* message ([type 'request]))]))))
                         wakeup-at)))

   (match-let-values ([(wakeup-at outgoing-events) (s (time-event final-wakeup))])
                     (check-pred null? outgoing-events "expected no outgoing events after all attempts exhausted")

                     ; init-state should trigger sending a discover on the transition to selecting.
                     (match-let-values ([(wakeup-at outgoing-events) (s (time-event wakeup-at))])
                                       (check-match outgoing-events
                                                    (list (send-msg (struct* message ([type 'discover])) 'broadcast))))))
  (test-case
   "An ACK with no renewal time picks a reasonable default")

  (test-case
   "Once t1 expires, a (unicast) DHCPREQUEST is sent"
   (define renewal-ms (seconds->milliseconds k-test-renewal-time))
   (define s (make-state-machine #:xid 42
                                 #:start-state (bound-state
                                                (lease-info (make-ip-address "192.168.11.12")
                                                            (make-ip-address "192.168.11.1"))
                                                renewal-ms
                                                (seconds->milliseconds k-test-rebinding-time))))
   ; the setup step made the transition to bound at 11100.
   ; no events until the renewal time.
   (for ([time-delta-ms (in-range 0 renewal-ms (seconds->milliseconds 100))])
     (let-values ([(_ events) (s (time-event (+ 11100 time-delta-ms)))])
       (check-true (empty? events))))
   ; now, renew.
   (let-values ([(_ events) (s (time-event (+ 11100 renewal-ms)))])
     (check-match events
                  (list (send-msg (struct* message ([type 'request])) (== canonical-server-ip))))))

  (test-case
   "When in renewing, if enough time has elapsed, rebinding is triggered"
   (define s (make-state-machine #:xid 42
                                 #:start-state (renewing-state
                                                (lease-info (make-ip-address "192.168.11.12")
                                                            (make-ip-address "192.168.11.1"))
                                                3000
                                                1000
                                                42)))
   (define-values (_ outgoing-events) (s (time-event 3200)))
   (check-match outgoing-events
                (list (send-msg (struct* message ([type 'request])) 'broadcast))))

  #;(test-case
     "When in renewing, renewal is re-attempted periodically")

  #;(test-case
     "When in renewing, unexpected packets are ignored")

  (test-case
   "When in renewing, a nak leads to reset"
   (define s (make-state-machine #:xid 42
                                 #:start-state (renewing-state
                                                (lease-info (make-ip-address "192.168.11.12")
                                                            (make-ip-address "192.168.11.1"))
                                                3000
                                                1000
                                                42)))
   (define-values (_ outgoing-events) (s (incoming 2000 canonical-server-ip
                                                   (message
                                                    'nak
                                                    42
                                                    1432
                                                    (number->ipv4-address 0)
                                                    (number->ipv4-address 0)
                                                    (number->ipv4-address 0)
                                                    (number->ipv4-address 0)
                                                    (list (message-option 'server-identifier canonical-server-ip))))))
   (check-match outgoing-events (list (iface-unbind))))

  (test-case
   "When in renewing, an ack moves back to bound"
   (define s (make-state-machine #:xid 42
                                 #:start-state (renewing-state
                                                (lease-info (make-ip-address "192.168.11.12")
                                                            (make-ip-address "192.168.11.1"))
                                                3000
                                                1000
                                                42)))

   (define-values (_ outgoing-events) (s (incoming 2000 canonical-server-ip
                                                   (message
                                                    'ack
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
                (list
                 (iface-bind
                  (lease-info
                   (== (make-ip-address "192.168.11.12"))
                   (== (make-ip-address "192.168.11.1")))))))

  #;(test-case
     "When in rebinding, rebinding is re-attempted periodically")

  #;(test-case
     "When in rebinding, unexpected packets are ignored")

  (test-case
   "When in rebinding, a nak leads to reset"
   (define s (make-state-machine #:xid 42
                                 #:start-state (rebinding-state
                                                (lease-info (make-ip-address "192.168.11.12")
                                                            (make-ip-address "192.168.11.1"))
                                                1000
                                                42)))
   (define-values (_ outgoing-events) (s (incoming 1100 canonical-server-ip
                                                   (message
                                                    'nak
                                                    42
                                                    1432
                                                    (number->ipv4-address 0)
                                                    (number->ipv4-address 0)
                                                    (number->ipv4-address 0)
                                                    (number->ipv4-address 0)
                                                    (list (message-option 'server-identifier canonical-server-ip))))))
   (check-match outgoing-events (list (iface-unbind))))

  (test-case
   "When in rebinding, an ack moves back to bound"
   (define s (make-state-machine #:xid 42
                                 #:start-state (rebinding-state
                                                (lease-info (make-ip-address "192.168.11.12")
                                                            (make-ip-address "192.168.11.1"))
                                                1000
                                                42)))

   (define-values (_ outgoing-events) (s (incoming 1100 canonical-server-ip
                                                   (message
                                                    'ack
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
                (list
                 (iface-bind
                  (lease-info
                   (== (make-ip-address "192.168.11.12"))
                   (== (make-ip-address "192.168.11.1")))))))

  #;(test-case
     "DHCPOFFER/ACK/NACK are discarded in BOUND state"
     (fail "TODO"))

  #;(test-case
     "Ensure all states handle an eventual timeout"))
