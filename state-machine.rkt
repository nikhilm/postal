#lang racket/base

(require racket/match)
(require racket/generator)
(require "message.rkt")

(provide tick)

; should thesejust be Racket evt events?
; we can't discriminate on them?
; we probably want to have a port that can take messages
; if we do want to sync on that.

(struct send-msg (msg to))
(struct time-instant (instant))

(struct state ())
(struct init-state state ())
(struct selecting-state state (offers timeout))

(define (tick state [event #f])
  (match state
    ['init (values 'selecting (make-dhcpdiscover 34))]))

; TODO: Accept configuration like mac address, xid, starting local time and so on.
(define (make-state-machine [start (init-state)])
  (generator ()
             (let loop ([current-state start])
               (match current-state
                 ; TODO: How to structure these? Not all yields will then change state.
                 [(init-state) (yield (list (send-msg (make-dhcpdiscover 34) 'broadcast)))
				;; the other weird part is, this yield is going to get the next incoming packet + time
				; which may already then want to advance the state of the loop (i.e. if we don't get any new packets, but do timeout)
				; however this code is currently going to switch to selecting state _after_ the generator resumes, not before.
				; so actually we may want to yield as the first step of entering a new state, and not the last step of the previous one?
				; if we want to put an instant in the future into the selecting state
				; we need a way to get the current instant
				; on every iteration
                               (loop (selecting-state null 10))]

                 [(selecting-state offers timeout)
                  ; accept either offers or a timeout
                  ; now here is the weird part of the yield
                  ; which is, it doesn't give anything back
                  ; how to handle that on the caller?
				  ; what if we always yield a list of events?
                  ;
                  (match (yield null)
                    [(and (struct message _) offer)
                     (loop
                      (selecting-state
                       (cons offer (selecting-state-offers current-state))
                       (selecting-state-timeout current-state)))]
                    ; todo: do we use time instants or elapsed?
                    ; probably instants
                    [(time-instant now)
                     ; TODO: Adjust time, capped to zero.
                     (if (>= now (selecting-state-timeout current-state))
                         (error "TODO select an offer")
                         (loop current-state))])]))))

(module+ test
  (require rackunit)
  ; will need to somehow mock sends and responses as well.
  ;also need to be able to test outgoing packets
  (test-case
   "Starting in init leads to a request to send DHCPDISCOVER"
   (define sm (make-state-machine))
   ; no input required
   (define next-event (sm))
   (check-match next-event
                (send-msg message to)
                (equal? (message-type message) 'discover)))

  ; will need to figure out how to test intermediate states
  ; while getting the machine to that state.
  (test-case
   "When in selecting, offers are considered for 10 seconds"
   (define sm (make-state-machine))
   ; no input required
   (sm)

   ; ok, how to model time and this offer stuff
   ; something like

   ; TODO: This is a made up packet that is not correct
   (sm (message 'offer 72 0 0 0 0 0))
   ; may end up being a "just call me back in some time"
   ; i think the problem with this generator based API is
   ; every iteration may not have something to send in, nor
   ; produce something useful. the send and receive are decoupled.
   (define next-event (sm 'timeout))
   (check-match next-event
                (send-msg message to)
                (equal? (message-type message) 'request))))
