#lang racket/base

(require racket/list)
(require racket/match)
(require net/ip)
(require "message.rkt")

(provide step make-state-machine send-msg incoming update)

; we can't discriminate on them?
; we probably want to have a port that can take messages
; if we do want to sync on that.

; outgoing event structs
(struct send-msg (msg to) #:transparent)

; incoming event structs
(struct incoming (hostname msg) #:transparent)
(struct update (sm next-timeout-instant outgoing) #:transparent)

(struct state () #:transparent)
(struct init-state state () #:transparent)
(struct selecting-state state (offers timeout) #:transparent)
(struct requesting-state (chosen timeout when) #:transparent)
; TODO: Put the lease information here since it is required for renewal.
; Likely will need to pull out into another struct.
; what is a good way to manage these towers of structs?
(struct bound-state (renew rebind) #:transparent)

(struct sm (current xid) #:transparent)

; this is the sort of thing that would be nice to not have to carry around everywhere
; TODO: Randomize xid
(define (make-state-machine [start (init-state)] [xid 0])
  (sm start xid))


(define (next-xid xid)
  (let ([new-xid (add1 xid)])
    (if (> new-xid 4294967295) 0 new-xid)))

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
(define (step-internal machine now incomings)
  ; TODO: contract
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
          (selecting-state (for/list ([ic (in-list incomings)])
                             ; TODO: Validate that the incoming packet is a offer
                             ic) timeout)
          timeout
          null))]

    [(requesting-state chosen timeout when)
     (if (>= now timeout)
         (error 'step "TODO: Handle not receiving ack")

         (match incomings
           ; TODO: Handle other messages
           ; TODO: Match with chosen
           [(list (incoming src msg))
            (let ([maybe-renew (optionsf msg 'renewal-time)]
                  [maybe-rebind (optionsf msg 'rebinding-time)])
              (if (and maybe-renew maybe-rebind)
                  (up-req (bound-state (+ when (seconds->milliseconds maybe-renew))
                                       (+ when (seconds->milliseconds maybe-rebind)))
                          (+ 5000 now)
                          null)
                  (error "TODO: Handle malformed message by going back to init or something")))]))]

    [(and orig-state (bound-state renew-instant rebind-instant))
     (cond
       [(>= now renew-instant) (error "TODO: Enter renewing")]
       [(>= now rebind-instant) (error "TODO: Handle rebinding")]
       ; TODO: Wish there was a way to say "nothing changed"
       [else (up-req orig-state (+ 5000 now) null)])]))

(define (step machine now incomings)
  (update-machine
   machine
   (step-internal machine now incomings)))

(define (request-from-offer xid offer)
  (match-let ([(struct incoming (hostname msg)) offer])
    (message 'request
             xid
             ; TODO: secs, should be the same as discover
             0
             0
             0
             0
             0
             (list (message-option 50 (message-yiaddr msg))
                   (message-option 54 (ip-address->number (make-ip-address hostname)))))))

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
    (incoming (ip-address->string sender) msg))

  (test-case
   "Starting in init leads to a request to send DHCPDISCOVER"
   (define s (make-state-machine))
   ; no input required
   (match-define (update _ _ (list next-event)) (step s 0 null))
   (check-match next-event
                (send-msg message to)
                (and (equal? (message-type message) 'discover)
                     (equal? to 'broadcast))))

  ; will need to figure out how to test intermediate states
  ; while getting the machine to that state.
  (test-case
   "When in selecting, offers are considered for 10 seconds"
   (define stepped
     (for/fold ([upd (update (make-state-machine) 0 null)])
               ([args (list '(7 ())
                            `(8000 (,(wrap-message (message 'offer 72 0 0 0 0 0 null))))
                            '(11000 ()))])
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
