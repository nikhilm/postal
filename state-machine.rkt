#lang racket/base

(require racket/contract)
(require racket/generator)
(require racket/list)
(require racket/match)
(require net/ip)
(require "logger.rkt")
(require "message.rkt")

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

(define (requesting-state-timeout now)
  (+ 10000 now))

(define (to-requesting-state when offer)
  (define event (yield (requesting-state-timeout when)
                       (list (send-msg (request-from-offer
                                        (next-xid!) offer) 'broadcast))))

  ; TODO: Propagate the current now via requesting to bound, since t1 and t2 are calculated from there
  ; TODO: Save the xid in the requesting-state so that the ack can be matched
  ((requesting-state when) event))

(define ((requesting-state request-send-instant) event)
  (define timeout-instant (requesting-state-timeout request-send-instant))
  (match event
    [(time-event now) #:when (>= now timeout-instant)
                      (error "Handle not receiving ack")]
    ; want to yield just a timeout and wait.
    [(time-event now) (error "TODO")]
    ; TODO: If this ack was not intended for us, reloop.
    [(and (incoming _ _ _) incom)
     (maybe-ack-to-bound incom request-send-instant)]))

(define/match (maybe-ack-to-bound incom when)
  ; TODO: Handle other kinds of messages instead of assuming this is an ack
  ; TODO: Match with chosen
  [((incoming in-time src msg) _)
   (let ([maybe-renew (optionsf msg 'renewal-time)]
         [maybe-rebind (optionsf msg 'rebinding-time)])
     ; TODO: Handle 'lease-time and store it!
     (if (and maybe-renew maybe-rebind)
         (let ([info (lease-info-from-ack msg)]
               [renew-instant (+ when (seconds->milliseconds maybe-renew))]
               [rebind-instant (+ when (seconds->milliseconds maybe-rebind))])
           (to-bound-state info renew-instant rebind-instant))
         (error "TODO: Handle malformed message by going back to init or something")))])

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
  (define next-evt (yield (+ 10000 request-instant) (list (send-msg (request-from-lease info request-xid) 'broadcast))))
  (log-postal-debug "next-evt when transitioning to rebinding state ~a" next-evt)
  ((rebinding-state info request-instant request-xid) next-evt))

; TODO: It seems like renewing and rebinding states have the same logic with slight differences. See what can be refactored.
(define ((rebinding-state info request-instant request-xid) first-incom)
  (let loop ([incoming-event first-incom])
    (match incoming-event
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

(define (request-to-server xid ciaddr)
  #|
  DHCPREQUEST generated during RENEWING state:

      'server identifier' MUST NOT be filled in, 'requested IP address'
      option MUST NOT be filled in, 'ciaddr' MUST be filled in with
      client's IP address. In this situation, the client is completely
      configured, and is trying to extend its lease. This message will
      be unicast, so no relay agents will be involved in its
      transmission.  Because 'giaddr' is therefore not filled in, the
      DHCP server will trust the value in 'ciaddr', and use it when
      replying to the client.

      A client MAY choose to renew or extend its lease prior to T1.  The
      server may choose not to extend the lease (as a policy decision by
      the network administrator), but should return a DHCPACK message
      regardless.
  |#
  (message 'request
           xid
           0
           ciaddr
           (number->ipv4-address 0)
           (number->ipv4-address 0)
           (number->ipv4-address 0)
           null))
#|
; TODO: Put the lease information here since it is required for renewal.
; Likely will need to pull out into another struct.
; what is a good way to manage these towers of structs?
(struct bound-state (renew rebind info) #:transparent)
(struct renewing-state (when rebind info) #:transparent)

    [(and orig-state (bound-state renew-instant rebind-instant info))
     (cond
       [(>= now renew-instant)
        (up-req (renewing-state now rebind-instant info)
                (+ 5000 now)
                ; Send a message to the server we last had a lease from.
                ; TODO: Record local time at which lease sent, to calculate expiration time!
                (list (send-msg
                       (request-to-server (sm-xid machine) (lease-info-client-addr info))
                       (lease-info-server-addr info))))]
       ; TODO: Wish there was a way to say "nothing changed"
       [else (up-req orig-state (+ 5000 now) null)])]

    [(and orig-state (renewing-state when rebind-instant info))
     (if
      ; TODO: There are a few timeouts to handle
      ; assuming there is a >60s delta between renewing and rebinding time,
      ; after 60s we should attempt to resend the request.
      ; same for rebinding, up until the lease time expires.
      (>= now rebind-instant)
      (error "TODO: Enter rebinding")
      (if (not incom)
          ; don't do anything until we hit rebinding if there are no incoming messages.
          (up-req orig-state (+ 5000 now) null)
          (begin
            (if (and (equal? (incoming-sender incom) (lease-info-server-addr info))
                     (equal? (message-type (incoming-msg incom)) 'ack))
                (maybe-ack-to-bound incom now when)
                (error "Need to handle not an ack")))))]))

|#

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
   (define s (make-state-machine #:xid 42 #:start-state (requesting-state 11100)))
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

#|
  (test-case
   "Handle transition to bound"
   (define sm (make-state-machine #:current (requesting-state #f 10 1000) #:xid 23))
   (define up (step sm 4 (incoming canonical-server-ip
                                   (message
                                    'ack
                                    23
                                    0
                                    (make-ip-address "172.16.1.182")
                                    (make-ip-address "172.16.1.182")
                                    (make-ip-address "172.16.1.1")
                                    (make-ip-address "0.0.0.0")
                                    (list
                                     (message-option 'server-identifier (make-ip-address "172.16.1.1"))
                                     (message-option 'lease-time 120)
                                     (message-option 'renewal-time 56)
                                     (message-option 'rebinding-time 101)
                                     (message-option 'subnet-mask (make-ip-address "255.255.255.0"))
                                     (message-option 'broadcast-address (make-ip-address "172.16.1.255"))
                                     (message-option 'router (make-ip-address "172.16.1.1"))
                                     (message-option 'dns-server (make-ip-address "172.16.1.1")))))))
   (match-define (update new-sm _ _) up)
   (check-match (sm-current new-sm)
                (bound-state 57000
                             102000
                             (lease-info client server))
                (and (equal? client (make-ip-address "172.16.1.182"))
                     (equal? server (make-ip-address "172.16.1.1"))))))
|#