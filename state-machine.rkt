#lang racket/base

(require racket/contract)
(require racket/generator)
(require racket/list)
(require racket/match)
(require net/ip)
(require "logger.rkt")
(require "message.rkt")

(provide step make-state-machine
         (struct-out send-msg)
         (struct-out iface-bind)
         (struct-out incoming)
         (struct-out lease-info)
         (struct-out time-event))

; fields are ip-address?
(struct lease-info (client-addr server-addr) #:transparent)

; outgoing event structs. to can be 'broadcast or ip-address?
(struct send-msg (msg to) #:transparent)

(struct iface-bind (info) #:transparent)

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

(define (make-state-machine #:xid [start-xid 0])
  ; should return a generator
  (generator (event)
             (parameterize ([xid start-xid])
               ; assume in initial state, although later we want to support starting from rebooting etc.
               ; could do that by accept formals in the generator
               ; need to figure out a better way to enter the right initial state.
               (match-let ([(time-event now) event])
                 (to-selecting-state now)))))

(define (requesting-timeout-instant n)
  (+ 10000 n))

(define (to-selecting-state now)
  (define (maybe-offer-list incom expected-xid)
    (if (reasonable-offer? incom expected-xid)
        (begin (log-postal-debug "considered reasonable ~a" incom)
               (list incom))
        null))

  (define timeout (+ 10000 now))
  (define xid (next-xid!))
  (let loop ([offers null]
             [outgoing-events (list (send-msg (make-dhcpdiscover xid) 'broadcast))])
    (log-postal-debug "selecting loop with offers ~a og ~a~n" offers outgoing-events)
    ; one of the tiny edge cases here is that the outgoing DHCPDISCOVER may not actually get
    ; sent right away, in which case the timeout is not allowing a full 10 seconds for discovery.
    (match (yield timeout outgoing-events)
      [(time-event now)
       (log-postal-debug "time-evt ~a~n" now)
       (if offers
           (to-requesting-state now (first offers))
           ; TODO: Client should probably retry, and this needs to indicate that somehow.
           (error "Need to handle not getting any offers"))]
      [(and (incoming _ _ _) incom)
       (log-postal-debug "incoming ~a~n" incom)
       ; accumulate offers, nothing to send.
       (loop (append offers (maybe-offer-list incom xid)) null)])))

(define (to-requesting-state when offer)
  (define timeout (requesting-timeout-instant when))
  ; TODO: Propagate the current now via requesting to bound, since t1 and t2 are calculated from there
  (define event (yield timeout
                       (list (send-msg (request-from-offer
                                        ; TODO: Save the xid in the requesting-state so that the ack can be matched
                                        (next-xid!) offer) 'broadcast))))

  (match event
    [(time-event now) #:when (>= now timeout)
                      (error "Handle not receiving ack")]
    ; want to yield just a timeout and wait.
    [(time-event now) (error "TODO")]
    [(and (incoming _ _ _) incom)
     (maybe-ack-to-bound incom when)]))

(define (step machine now incoming)
  ; should assume machine is a generator stopped at a yield for now + incoming
  ; so it can just apply
  (machine now incoming))

(define (maybe-ack-to-bound incom when)
  (match incom
    ; TODO: Handle other kinds of messages instead of assuming this is an ack
    ; TODO: Match with chosen
    [(incoming in-time src msg)
     (let ([maybe-renew (optionsf msg 'renewal-time)]
           [maybe-rebind (optionsf msg 'rebinding-time)])
       ; TODO: Handle 'lease-time and store it!
       (if (and maybe-renew maybe-rebind)
           (let ([info (lease-info-from-ack msg)]
                 [renew-instant (+ when (seconds->milliseconds maybe-renew))]
                 [rebind-instant (+ when (seconds->milliseconds maybe-rebind))])
             (to-bound-state info renew-instant rebind-instant))
           (error "TODO: Handle malformed message by going back to init or something")))]))

(define (to-bound-state info renew-instant rebind-instant)
  ; for now, hang out in bound forever.
  (let loop ([next-instant renew-instant]
             [outgoing (list (iface-bind info))])
    (match (yield next-instant outgoing)
      [(time-event now)
       (log-postal-warning "Looping forever in bound state!")
       (loop (+ 5000 now) null)]
      [_ (error "Don't know what to do with a packet when in bound.")])))

(define (lease-info-from-ack msg)
  (unless (eq? (message-type msg) 'ack)
    (error "Not a DHCPACK message"))

  (lease-info (message-yiaddr msg)
              (optionsf msg 'server-identifier)))

(define (seconds->milliseconds sec)
  (* 1000 sec))

(define (reasonable-offer? incom expected-xid)
  (match incom
    [(incoming _ src (struct* message ([type 'offer] [xid (== expected-xid)])))
     #t]
    [_ #f]))

(define (request-from-offer xid offer)
  (match-let ([(struct incoming (_ sender msg)) offer])
    (message 'request
             xid
             ; TODO: secs, should be the same as discover
             0
             (number->ipv4-address 0)
             (number->ipv4-address 0)
             (number->ipv4-address 0)
             (number->ipv4-address 0)
             (list (message-option 50 (message-yiaddr msg))
                   (message-option 54 sender)))))

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

  (define (canonical-server-ip)
    (make-ip-address "192.168.11.1"))

  (define (extract-option options tag)
    (findf (lambda (opt)
             (equal? (message-option-tag opt) tag))
           options))

  (define (wrap-message now msg [sender (canonical-server-ip)])
    (incoming now sender msg))

  (define (make-bound-machine)
    (define s (make-state-machine #:xid 42))
    (s (time-event 0))
    (s (wrap-message 1000 (message 'offer
                                   42
                                   0
                                   (number->ipv4-address 0)
                                   (make-ip-address "192.168.11.12")
                                   (make-ip-address "192.168.11.1")
                                   (number->ipv4-address 0)
                                   (list (message-option 'renewal-time k-test-renewal-time)
                                         (message-option 'rebinding-time k-test-rebinding-time)
                                         (message-option 'lease-time k-test-lease-time)
                                         (message-option 'server-identifier (make-ip-address "192.168.11.1"))))))

    (let-values ([(_ events) (s (time-event 11000))])
      (check-equal? events (list
                            (send-msg
                             (message
                              'request
                              43
                              0
                              (make-ip-address "0.0.0.0")
                              (make-ip-address "0.0.0.0")
                              (make-ip-address "0.0.0.0")
                              (make-ip-address "0.0.0.0")
                              (list
                               (message-option 50 (make-ip-address "192.168.11.12"))
                               (message-option 54 (make-ip-address "192.168.11.1"))))
                             'broadcast))))

    (define-values (_ outgoing-events)
      (s (wrap-message 11100 (message 'ack
                                      42
                                      0
                                      (make-ip-address "192.168.11.12")
                                      (make-ip-address "192.168.11.12")
                                      (make-ip-address "192.168.11.1")
                                      (number->ipv4-address 0)
                                      (list (message-option 'renewal-time 1800)
                                            (message-option 'rebinding-time 3150)
                                            (message-option 'lease-time 3600)
                                            (message-option 'server-identifier (make-ip-address "192.168.11.1")))))))

    (check-match outgoing-events
                 (list (iface-bind (lease-info caddr saddr)))
                 (and (equal? caddr (make-ip-address "192.168.11.12"))
                      (equal? saddr (make-ip-address "192.168.11.1"))))

    s)

  (test-case
   "Starting in init leads to a request to send DHCPDISCOVER"
   (define s (make-state-machine))
   ; no input required
   (define-values (_ events) (s (time-event 0)))
   (check-match events
                (list (send-msg message to))
                (and (equal? (message-type message) 'discover)
                     (equal? to 'broadcast))))

  ; will need to figure out how to test intermediate states
  ; while getting the machine to that state.
  (test-case
   "When in selecting, offers are considered for 10 seconds"
   (define s (make-state-machine #:xid 34))
   (s (time-event 0))
   (let-values ([(_ events) (s (wrap-message 8000 (message 'offer
                                                           34
                                                           0
                                                           (number->ipv4-address 0)
                                                           (make-ip-address "192.168.11.3")
                                                           (number->ipv4-address 0)
                                                           (number->ipv4-address 0)
                                                           null)))])
     (check-equal? events null))

   (let-values ([(_ events) (s (wrap-message 9900 (message 'offer
                                                           34
                                                           0
                                                           (number->ipv4-address 0)
                                                           (make-ip-address "192.168.11.6")
                                                           (number->ipv4-address 0)
                                                           (number->ipv4-address 0)
                                                           null) (make-ip-address "192.168.11.99")))])
     (check-equal? events null))

   (let-values ([(_ events) (s (time-event 11000))])
     (check-match events
                  (list (send-msg (struct* message ([type 'request] [options opts])) 'broadcast))
                  (and (equal? (message-option-value (extract-option opts 54))
                               (canonical-server-ip))
                       (equal? (message-option-value (extract-option opts 50))
                               (make-ip-address "192.168.11.3"))))))

  (test-case
   "An offer with a non-matching xid is ignored."
   (define s (make-state-machine #:xid 34))
   (s (time-event 7))
   (s (wrap-message 4000 (message 'offer
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
   ; re-use the helper as a test.
   (make-bound-machine))

  (test-case
   "Stays in BOUND until t1"
   (define s (make-bound-machine))
   (check-equal? 2 2)))

#|
  (test-case
   "Handle transition to bound"
   (define sm (make-state-machine #:current (requesting-state #f 10 1000) #:xid 23))
   (define up (step sm 4 (incoming (canonical-server-ip)
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