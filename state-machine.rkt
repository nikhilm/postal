#lang racket/base

(require racket/contract)
(require racket/list)
(require racket/match)
(require net/ip)
(require racket/system)
(require "message.rkt")

(provide step make-state-machine send-msg
         (struct-out incoming)
         (struct-out update))

; fields should be make-ip-address addresses
(struct lease-info (client-addr server-addr) #:transparent)

; we can't discriminate on them?
; we probably want to have a port that can take messages
; if we do want to sync on that.

; outgoing event structs
(struct send-msg (msg to) #:transparent)

; incoming event structs
(struct incoming (sender msg) #:transparent)
(struct update (sm next-timeout-instant outgoing) #:transparent)

(struct state () #:transparent)
(struct init-state state () #:transparent)
(struct selecting-state state (offers timeout) #:transparent)
(struct requesting-state (chosen timeout when) #:transparent)
; TODO: Put the lease information here since it is required for renewal.
; Likely will need to pull out into another struct.
; what is a good way to manage these towers of structs?
(struct bound-state (renew rebind info) #:transparent)
(struct renewing-state (when rebind info) #:transparent)

(struct sm (current xid) #:transparent)

; this is the sort of thing that would be nice to not have to carry around everywhere
; TODO: Randomize xid
(define (make-state-machine [start (init-state)] [xid 0])
  (sm start xid))

(define (next-xid xid)
  (modulo (add1 xid) 4294967295))

(struct up-req (new-state timeout outgoing) #:transparent)

(define/match (update-machine machine req)
  ; TODO: Probably a more succinct way to write this.
  ; Only bump the xid if there are outgoing messages
  [((sm _ xid) (up-req new-state timeout outgoing)) (define nxid (if (null? outgoing) xid (next-xid xid)))
                                                    (update (sm new-state nxid)
                                                            timeout outgoing)])

; TODO: Accept configuration like mac address, xid, starting local time and so on.
; TODO: Consider making incoming just a single message, since state can change based on a single message.
; unlike OOP 2 functions for stuff (one to send in new data, one to receive events)
; i think we may be able to get away with just one, as long as we pass around some stuff internally.
; -> update
(define/contract (step-internal machine now incom)
  (sm? real? (or/c incoming? #f) . -> . up-req?)
  (match (sm-current machine)
    [(init-state) (up-req
                   ; TODO: Jitter the timeout
                   (selecting-state null (+ 10000 now))
                   (+ 10000 now)
                   (list (send-msg (make-dhcpdiscover (sm-xid machine)) 'broadcast)))]

    ; TODO: Response xid validation.
    [(selecting-state offers timeout)
     (if (>= now timeout)
         ; TODO: Pick the best offer from offers
         ; TODO: Handle no offers - client has to retry with some timeout
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

         (up-req
          ; TODO: Validate that the incoming packet is a offer
          (selecting-state (append offers (if incom (list incom) null)) timeout)
          timeout
          null))]

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
                    (printf "COMMAND IS ~a~n" cmd)
                    (system cmd)
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
                       ; Perhaps we are not allowed to send non-broadcast packets if we don't have an IP.
                       (ip-address->string (lease-info-server-addr info)))))]
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
            (printf "INCOMING MESSAGE IS ~v~n" incom)
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
             0
             0
             0
             0
             (list (message-option 50 (message-yiaddr msg))
                   (message-option 54 (ip-address->number sender))))))

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
           (ip-address->number ciaddr)
           0
           0
           0
           null))

(define (lease-info-from-ack msg)
  (unless (eq? (message-type msg) 'ack)
    (error "Not a DHCPACK message"))

  (lease-info (number->ipv4-address (message-yiaddr msg))
              (optionsf msg 'server-identifier)))

(define (seconds->milliseconds sec)
  (* 1000 sec))

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
     (for/fold ([upd (update (make-state-machine) 0 #f)])
               ([args (list '(7 #f)
                            `(8000 ,(wrap-message (message 'offer 72 0 0 0 0 0 null)))
                            '(11000 #f))])
       (apply step (update-sm upd) args)))
   ; probably should use some check form that prints each substep diff
   (check-match (first (update-outgoing stepped))
                (send-msg message to)
                (and (equal? (message-type message) 'request)
                     (equal? to 'broadcast)
                     (equal? (message-option-value (extract-option message 54))
                             (ip-address->number (canonical-server-ip))))))

  (test-case
   "Handle transition to bound"
   (fail "TODO transition to bound")))
