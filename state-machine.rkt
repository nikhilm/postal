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

(struct state ())
(struct init-state state ())
(struct selecting-state state (offers timeout))
(struct requesting-state ())

(struct sm (current xid))

; this is the sort of thing that would be nice to not have to carry around everywhere
; TODO: Randomize xid
(define (make-state-machine [start (init-state)] [xid 0])
  (sm start xid))

(define/match (bump-update-xid upd)
  [((update sm tm out)) (update (update-xid sm) tm out)])

(define (update-xid s)
  (let ([new-xid (add1 (sm-xid s))])
    (struct-copy sm s [xid (if (> new-xid 4294967295) 0 new-xid)])))

(define (update-state s new-state)
  (struct-copy sm s [current new-state]))

; TODO: Accept configuration like mac address, xid, starting local time and so on.
; TODO: Consider making incoming just a single message, since state can change based on a single message.
; unlike OOP 2 functions for stuff (one to send in new data, one to receive events)
; i think we may be able to get away with just one, as long as we pass around some stuff internally.
; -> update
(define (step machine now incoming)
  (bump-update-xid
   ; TODO: contract
   (match (sm-current machine)
     [(init-state) (update
                    (update-state machine (selecting-state null (+ 10000 now)))
                    (+ 10000 now)
                    (list (send-msg (make-dhcpdiscover (sm-xid machine)) 'broadcast)))]

     ; TODO: Response xid validation.
     [(selecting-state offers timeout)
      (if (>= now timeout)
          ; TODO: Pick the best offer from offers
          ; TODO: Handle no offers - client has to retry with some timeout
          (update
           (update-state machine (requesting-state))
           ; if the server does not respond within this time, we need to do more stuff
           ; TODO: Handle this case
           (+ 50000 now)
           ; TODO: Set options requested IP and server identifier
           (list (send-msg (request-from-offer
                            (sm-xid machine) (first offers)) 'broadcast)))

          (update
           (update-state machine (selecting-state (for/list ([ic (in-list incoming)])
                                                    ; TODO: Validate that the incoming packet is a offer
                                                    ic) timeout))
           timeout
           null))])))

(define (request-from-offer xid offer)
  (match-let ([(struct incoming (hostname msg)) offer])
    (with-options (message
                   'request
                   xid
                   ; secs, should be the same as discover
                   0
                   0
                   0
                   0
                   0
                   null)
      (list (message-option 50 (message-yiaddr msg))
            (message-option 54 (ip-address->number (make-ip-address hostname)))))))


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
                             (ip-address->number (canonical-server-ip)))))))
