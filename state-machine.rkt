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

(struct sm (current))

(define (make-state-machine [start (init-state)])
  (sm start))

; TODO: Accept configuration like mac address, xid, starting local time and so on.
(define (update-state machine now incoming)
  ; TODO: contract
  (match (sm-current machine)
    [(init-state) (values
                   (sm (selecting-state null (+ 10 now)))
                   (list (send-msg (make-dhcpdiscover 34) 'broadcast)))]

    [(selecting-state offers timeout)
     (if (>= now timeout)
         ; TODO: Pick the best offer from offers
         ; TODO: Handle no offers
         (values
          (sm (requesting-state))
          (list "should-be-dhcprequest"))

         (values
          (sm (selecting-state (for ([msg (in-list incoming)])
                                 ; TODO: Validate that the incoming packet is a offer
                                 msg) timeout))
          null))]))


(module+ test
  (require rackunit)
  ; will need to somehow mock sends and responses as well.
  ;also need to be able to test outgoing packets
  (test-case
   "Starting in init leads to a request to send DHCPDISCOVER"
   (define sm (make-state-machine))
   ; no input required
   (match-define-values (_ (list next-event)) (update-state sm 0 null))
   (check-match next-event
                (send-msg message to)
                (equal? (message-type message) 'discover)))

  ; will need to figure out how to test intermediate states
  ; while getting the machine to that state.
  (test-case
   "When in selecting, offers are considered for 10 seconds"
   ; can we use the threading module to simplify the nested lets?
   (let*-values ([(sm) (make-state-machine)]
                 [(sm _) (update-state sm 7 null)]
                 ; TODO: This is a made up packet that is not correct
                 [(sm _) (update-state sm 8 (list (message 'offer 72 0 0 0 0 0)))]
				[(sm outputs) (update-state sm 18 (list (message 'offer 72 0 0 0 0 0)))])
     (check-match (first outputs)
                  (send-msg message to)
                  (equal? (message-type message) 'request)))))
