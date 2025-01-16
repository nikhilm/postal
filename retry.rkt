#lang racket/base

(require racket/match)

(provide
 make-retry
 try-again
 (rename-out [tm-fail fail]))

; 1 means no jitter because (random k) returns an int 0 <= n < k
(define (make-retry now initial-duration #:max-attempts max-attempts #:jitter [jitter 1])
  (define/match (timer instant)
    ; case where not enough time has elapsed.
    [(instant) #:when (instant . < . (+ now initial-duration))
               ; no attempt was used yet. request next wakeup after initial duration.
               (try-again (make-retry now initial-duration #:max-attempts max-attempts #:jitter jitter) (+ now initial-duration))]
    ; time has elapsed. determine based on attempts left.
    [(instant)
     (if (zero? max-attempts)
         (tm-fail)
         ; remember, next-instant may be _far_ after initial-expiry-instant.
         ; in that case, do we adjust for that, or add to that?
         ; right now, adding to that.
         ; TODO: jitter
         (let ([next-instant (+ instant (* initial-duration 2) (random jitter))])
           (try-again (make-retry instant (* initial-duration 2) #:max-attempts (sub1 max-attempts) #:jitter jitter) next-instant)))])

  timer)

; The timer can be used in case the caller is not satisfied with the result.
; oh, but what if this was the last attempt? Well in that case, it will immediately fail the next time.
(struct try-again (timer next-instant) #:transparent)
(struct tm-fail () #:transparent)

(module+ test
  (require racket/match)
  (require rackunit)
  (test-case
   "Basic timeout"

   (define timer (make-retry 5000 50 #:max-attempts 3))
   ; this was before the first expected timeout, so should return try-again with no attempt deduction.
   (match-define (try-again timer1 next-inst1) (timer 5023))
   (check-eq? next-inst1 5050)

   (match-define (try-again timer2 next-inst2) (timer1 next-inst1))
   (check-eq? next-inst2 5150)

   (match-define (try-again timer3 next-inst3) (timer2 (add1 next-inst2)))
   (check-eq? next-inst3 5351)

   ; another spurious wakeup.
   (match-define (try-again timer4 next-inst4) (timer3 (sub1 next-inst3)))
   (check-eq? next-inst4 5351)

   (check-match (timer4 (add1 next-inst4)) (tm-fail)))

  (test-case
   "Jitter"
   (let ([max-attempts 7]
         [start-inst 5000]
         [start-duration 50]
         [jitter 10])
     (define-values (timer next-inst)
       (for/fold ([timer (make-retry start-inst start-duration #:max-attempts max-attempts #:jitter jitter)]
                  [inst 5001])
                 ([i (in-range 7)] )
         (match-define (try-again next-timer next-inst) (timer inst))
         (check-= next-inst (+ inst (* start-duration (expt 2 i))) 10)
         (values next-timer next-inst)))
     (check-match (timer (add1 next-inst)) (tm-fail)))))
