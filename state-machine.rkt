#lang racket/base

(require racket/list)
(require racket/match)
(require racket/generator)
(require "message.rkt")

(provide update-state)

; should thesejust be Racket evt events?
; we can't discriminate on them?
; we probably want to have a port that can take messages
; if we do want to sync on that.

(struct send-msg (msg to))
(struct time-instant (instant))

(struct state ())
(struct init-state state ())
(struct selecting-state state (offers timeout))
(struct requesting-state ())

(struct sm (current xid))

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
          (list (send-msg (message
                           'request
                           (sm-xid machine)
                           ; secs, should be the same as discover
                           0
                           0
                           0
                           0
                           0) 'broadcast)))

         (values
          (update-xid machine (selecting-state (for ([msg (in-list incoming)])
                                                 ; TODO: Validate that the incoming packet is a offer
                                                 msg) timeout))
          null))]))


(module+ test
  (require rackunit)
  ; will need to somehow mock sends and responses as well.
  ;also need to be able to test outgoing packets
  (test-case
   "Starting in init leads to a request to send DHCPDISCOVER"
   (define s (make-state-machine))
   ; no input required
   (match-define-values (_ (list next-event)) (update-state s 0 null))
   (check-match next-event
                (send-msg message to)
                (equal? (message-type message) 'discover)))

  ; will need to figure out how to test intermediate states
  ; while getting the machine to that state.
  (test-case
   "When in selecting, offers are considered for 10 seconds"
   ; can we use the threading module to simplify the nested lets?
   (let*-values ([(machine) (make-state-machine)]
                 [(machine _) (update-state machine 7 null)]
                 ; TODO: This is a made up packet that is not correct
                 [(machine _) (update-state machine 8 (list (message 'offer 72 0 0 0 0 0)))]
                 [(machine outputs) (update-state machine 18 (list (message 'offer 72 0 0 0 0 0)))])
     (check-match (first outputs)
                  (send-msg message to)
                  (and (equal? (message-type message) 'request)
                       #t)))))
