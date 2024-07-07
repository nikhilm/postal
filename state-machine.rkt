#lang racket/base

(require racket/contract)
(require racket/list)
(require racket/match)
(require net/ip)
(require racket/system)
(require "logger.rkt")
(require "message.rkt")

(provide step make-state-machine send-msg
         (struct-out incoming)
         (struct-out update))

; fields are ip-address?
(struct lease-info (client-addr server-addr) #:transparent)

; outgoing event structs. to can be 'broadcast or ip-address?
(struct send-msg (msg to) #:transparent)

; incoming event structs
(struct incoming (sender msg) #:transparent)
(struct update (sm next-timeout-instant outgoing) #:transparent)

; states
(struct state () #:transparent)
(struct init-state state () #:transparent)
(struct selecting-state state (offers timeout expected-xid) #:transparent)
(struct requesting-state (chosen timeout when) #:transparent)
; TODO: Put the lease information here since it is required for renewal.
; Likely will need to pull out into another struct.
; what is a good way to manage these towers of structs?
(struct bound-state (renew rebind info) #:transparent)
(struct renewing-state (when rebind info) #:transparent)

(struct sm (current xid) #:transparent)

; this is the sort of thing that would be nice to not have to carry around everywhere
; TODO: Randomize xid
(define (make-state-machine #:current [current (init-state)] #:xid [xid 0])
  (sm current xid))

(define (next-xid xid)
  (modulo (add1 xid) 4294967295))

(struct up-req (new-state timeout outgoing) #:transparent)

(define/match (update-machine machine req)
  ; TODO: Probably a more succinct way to write this.
  ; Only bump the xid if there are outgoing messages
  [((sm _ xid) (up-req new-state timeout outgoing)) (define nxid (if (null? outgoing) xid (next-xid xid)))
                                                    (update (sm new-state nxid)
                                                            timeout outgoing)])

; unlike OOP 2 functions for stuff (one to send in new data, one to receive events)
; i think we may be able to get away with just one, as long as we pass around some stuff internally.
; -> update
(define/contract (step-internal machine now incom)
  (sm? real? (or/c incoming? #f) . -> . up-req?)
  (match (sm-current machine)
    [(init-state) (up-req
                   ; TODO: Jitter the timeout
                   (selecting-state null (+ 10000 now) (sm-xid machine))
                   (+ 10000 now)
                   (list (send-msg (make-dhcpdiscover (sm-xid machine)) 'broadcast)))]

    ; TODO: Response xid validation.
    [(selecting-state offers timeout expected-xid)
     (if (>= now timeout)
         (if offers
             (up-req
              ; TODO: Jitter the timeout
              ; It is quite possible for the client to not send out the request offer "right away" relative to `now`
              ; but it seems pretty unlikely in practice.
              (requesting-state (first offers) (+ 10000 now) now)
              ; if the server does not respond within this time, we need to do more stuff
              ; TODO: Handle this case
              (+ 10000 now)
              ; TODO: Set options requested IP and server identifier
              ; TODO: Propagate the current now via requesting to bound, since t1 and t2 are calculated from there
              (list (send-msg (request-from-offer
                               ; TODO: Save the xid in the requesting-state so that the ack can be matched
                               (sm-xid machine) (first offers)) 'broadcast)))
             ; TODO: Client should probably retry, and this needs to indicate that somehow.
             (error "Need to handle this state"))

         (if (reasonable-offer? incom expected-xid)
             (begin
               (log-postal-debug "considered reasonable ~a" incom)
               (up-req
                ; TODO: Validate that the incoming packet is a offer
                (selecting-state (append offers (list incom)) timeout expected-xid)
                timeout
                null))
             (begin
               (log-postal-warning "Dropping incoming ~a since it is not reasonable." incom)
               (up-req (selecting-state offers timeout expected-xid)
                       timeout
                       null))))]

    [(requesting-state chosen timeout when)
     ; TODO: Since this timeout is relative to when, but not dependent on the server response the struct doesn't need to store it.
     (if (>= now timeout)
         (error 'step "TODO: Handle not receiving ack")

         (match incom
           ; TODO: Handle other kinds of messages instead of assuming this is an ack
           ; TODO: Match with chosen
           [(incoming src msg)
            (let ([maybe-renew (optionsf msg 'renewal-time)]
                  [maybe-rebind (optionsf msg 'rebinding-time)])
              ; TODO: Handle 'lease-time and store it!
              (if (and maybe-renew maybe-rebind)
                  ; TODO: Move this out of the state-machine and into the client, which needs to understand outgoing
                  ; beyond just packets to send, or provide another interface to the state machine or something (i.e. callbacks).
                  (let* ([info (lease-info-from-ack msg)]
                         [cmd (format "sudo ip addr add ~a/24 dev veth1" (ip-address->string (lease-info-client-addr info)))])
                    (log-postal-info "Running ~a" cmd)
                    ; (system cmd)
                    (up-req (bound-state (+ when (seconds->milliseconds maybe-renew))
                                         (+ when (seconds->milliseconds maybe-rebind))
                                         info)
                            (+ 5000 now)
                            null))
                  (error "TODO: Handle malformed message by going back to init or something")))]))]

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
                (error "Go back to bound")
                (error "Need to handle not an ack")))))]))

(define/contract (step machine now incom)
  (sm? real? (or/c incoming? #f) . -> . update?)
  (update-machine
   machine
   (step-internal machine now incom)))

(define (request-from-offer xid offer)
  (match-let ([(struct incoming (sender msg)) offer])
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

(define (lease-info-from-ack msg)
  (unless (eq? (message-type msg) 'ack)
    (error "Not a DHCPACK message"))

  (lease-info (message-yiaddr msg)
              (optionsf msg 'server-identifier)))

(define (seconds->milliseconds sec)
  (* 1000 sec))

(define (reasonable-offer? incom expected-xid)
  (match incom
    [(incoming src (struct* message ([type 'offer] [xid (== expected-xid)])))
     #t]
    [_ #f]))

(module+ test
  (require rackunit)
  (require net/ip)

  (define (canonical-server-ip)
    (make-ip-address "192.168.11.1"))

  (define (extract-option msg tag)
    (findf
     (lambda (opt)
       (equal? (message-option-tag opt) tag))
     (message-options msg)))

  (define (wrap-message msg [sender (canonical-server-ip)])
    (incoming sender msg))

  (define (multi-step sm steps)
    ; step sm by applying each entry in steps.
    ; return the final update
    (for/fold ([upd (update sm 0 #f)])
              ([step-args (in-list steps)])
      (apply step (update-sm upd) step-args)))

  (test-case
   "Starting in init leads to a request to send DHCPDISCOVER"
   (define s (make-state-machine))
   ; no input required
   (match-define (update _ _ (list next-event)) (step s 0 #f))
   (check-match next-event
                (send-msg message to)
                (and (equal? (message-type message) 'discover)
                     (equal? to 'broadcast))))

  ; will need to figure out how to test intermediate states
  ; while getting the machine to that state.
  (test-case
   "When in selecting, offers are considered for 10 seconds"
   (define stepped
     (multi-step (make-state-machine #:xid 34)
                 (list '(7 #f)
                       `(8000 ,(wrap-message (message 'offer
                                                      34
                                                      0
                                                      (number->ipv4-address 0)
                                                      (number->ipv4-address 0)
                                                      (number->ipv4-address 0)
                                                      (number->ipv4-address 0)
                                                      null)))
                       '(11000 #f))))
   ; probably should use some check form that prints each substep diff
   (check-match (first (update-outgoing stepped))
                (send-msg message to)
                (and (equal? (message-type message) 'request)
                     (equal? to 'broadcast)
                     (equal? (message-option-value (extract-option message 54))
                             (canonical-server-ip)))))

  (test-case
   "An offer with a non-matching xid is ignored."
   ; start xid at 34 and then send something lower.
   (define stepped
     (multi-step (make-state-machine #:current (selecting-state null 10000 34) #:xid 34)
                 (list `(4000 ,(wrap-message (message 'offer
                                                      72
                                                      0
                                                      (number->ipv4-address 0)
                                                      (number->ipv4-address 0)
                                                      (number->ipv4-address 0)
                                                      (number->ipv4-address 0)
                                                      null))))))
   (check-exn exn:fail? (lambda () (step (update-sm stepped) 11000 #f))))

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
