#lang racket/base

(require racket/list)
(require racket/match)
(require "message.rkt")

(provide update-state make-state-machine)

; we can't discriminate on them?
; we probably want to have a port that can take messages
; if we do want to sync on that.

; outgoing event structs
(struct send-msg (msg to) #:transparent)

; incoming event structs
(struct incoming (hostname msg))

(struct state ())
(struct init-state state ())
(struct selecting-state state (offers timeout))
(struct requesting-state ())

(struct sm (current xid))

; OK. problems to solve:
; 1. the state machine needs to know the sender of the message too
; 2. the states are having to juggle a lot of fields?
; 3. can we at least make it more readable?

; TODO: Stash the xid somewhere.
; this is the sort of thing that would be nice to not have to carry around everywhere
; TODO: Randomize xid
(define (make-state-machine [start (init-state)] [xid 0])
  (sm start xid))

(define (update-xid s state)
  (let ([new-xid (add1 (sm-xid s))])
    (sm state (if (> new-xid 4294967295) 0 new-xid))))

; TODO: Accept configuration like mac address, xid, starting local time and so on.
(define (update-state machine now incoming)
  ; TODO: contract
  (match (sm-current machine)
    [(init-state) (values
                   (update-xid machine (selecting-state null (+ 10 now)))
                   (list (send-msg (make-dhcpdiscover (sm-xid machine)) 'broadcast)))]

    ; TODO: Response xid validation.
    [(selecting-state offers timeout)
     (if (>= now timeout)
         ; TODO: Pick the best offer from offers
         ; TODO: Handle no offers - client has to retry with some timeout
         (values
          (update-xid machine (requesting-state))
          ; TODO: Set options requested IP and server identifier
          (list (send-msg (request-from-offer
                           (sm-xid machine) (first offers)) 'broadcast)))

         (values
          (update-xid machine (selecting-state (for/list ([ic (in-list incoming)])
                                                 ; TODO: Validate that the incoming packet is a offer
                                                 ic) timeout))
          null))]))

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
            (message-option 54 hostname)))))


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
    (incoming (ip-address->number sender) msg))

  (test-case
   "Starting in init leads to a request to send DHCPDISCOVER"
   (define s (make-state-machine))
   ; no input required
   (match-define-values (_ (list next-event)) (update-state s 0 null))
   (check-match next-event
                (send-msg message to)
                (and (equal? (message-type message) 'discover)
                     (equal? to 'broadcast))))

  ; will need to figure out how to test intermediate states
  ; while getting the machine to that state.
  (test-case
   "When in selecting, offers are considered for 10 seconds"
   ; can we use the threading module to simplify the nested lets?
   (let*-values ([(machine) (make-state-machine)]
                 [(machine _) (update-state machine 7 null)]
                 ; TODO: This is a made up packet that is not correct
                 [(machine _) (update-state machine 8
                                            (list
                                             (wrap-message (message 'offer 72 0 0 0 0 0 null))))]
                 [(_ outputs) (update-state machine 18
                                            (list
                                             (wrap-message (message 'offer 72 0 0 0 0 0 null))))])
     ; probably should use some check form that prints each substep diff
     (check-match (first outputs)
                  (send-msg message to)
                  (and (equal? (message-type message) 'request)
                       (equal? to 'broadcast)
                       (equal? (message-option-value (extract-option message 54))
                               (ip-address->number (canonical-server-ip))))))))
