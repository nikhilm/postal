#lang racket/base

(require racket/contract)
(require racket/match)
(require "logger.rkt")

;; Thinking about the retries a bit more, there are 2 policies
;; 1. sorta exponential backoff, which is what is implemented below.
;; 2. working backwards from a specific instant, where the next deadline is half of now to the instant, but with a minimum bound.
;; i think we could encode this as another policy here, so that renew/rebind don't need all that logic in their body.
;;
;; the caller also has to make two decisions right now:
;; 1. does "now" exceed the current try's deadline?
;; 2. was this the last try?
;; this also applies to both policies.

; Create a retry policy with max attempts, base timeout (ms), and jitter range (ms)
(provide (struct-out retry-policy)

         ; Start a new retry sequence
         start-retry

         ; Check if current attempt has expired
         expired?

         ; Get next retry state, or #f if exhausted
         next-retry

         ; Get deadline for current attempt
         (rename-out [retry-state-deadline get-deadline]))

; Configuration for retry behavior
(struct retry-policy
        (max-attempts ; Maximum number of attempts
         base-timeout ; Base timeout in milliseconds
         jitter-range)
  #:transparent) ; +/- jitter range in milliseconds

; Current state of a retry operation
(struct retry-state
        (policy ; The retry-policy being used
         attempt-num ; Current attempt number (1-based)
         deadline)
  #:transparent) ; When current attempt expires

; Start a new retry sequence
(define/contract (start-retry policy now)
  (retry-policy? exact-nonnegative-integer? . -> . retry-state?)
  (define timeout (calculate-timeout policy 1))
  (log-postal-debug "Starting new retry sequence with policy ~a at ~a" policy now)
  (retry-state policy 1 (+ now timeout)))

(define (calculate-timeout policy attempt-num)
  (define base-timeout (retry-policy-base-timeout policy))
  (define jitter-range (retry-policy-jitter-range policy))
  (define jitter
    (if (zero? jitter-range)
        0
        (- (random jitter-range) (quotient jitter-range 2))))
  (define timeout (+ (* base-timeout (expt 2 (sub1 attempt-num))) jitter))
  (log-postal-debug "Calculated timeout for attempt ~a: base=~ams, jitter=~ams, total=~ams"
                    attempt-num
                    base-timeout
                    jitter
                    timeout)
  timeout)

(define/contract (next-retry state now)
  (retry-state? exact-nonnegative-integer? . -> . (or/c #f retry-state?))
  (define policy (retry-state-policy state))
  (define next-attempt (add1 (retry-state-attempt-num state)))

  (if (> next-attempt (retry-policy-max-attempts policy))
      (begin
        (log-postal-debug "No more retries available after attempt ~a"
                          (retry-state-attempt-num state))
        #f) ; No more retries
      (let ([next-deadline (+ now (calculate-timeout policy next-attempt))])
        (log-postal-debug "Moving to retry attempt ~a with next deadline ~a"
                          next-attempt
                          next-deadline)
        (retry-state policy next-attempt next-deadline))))

(define/contract (expired? state now)
  (retry-state? exact-nonnegative-integer? . -> . boolean?)
  (define expired? (>= now (retry-state-deadline state)))
  (when expired?
    (log-postal-debug "Retry timeout expired at ~a (deadline was ~a)"
                      now
                      (retry-state-deadline state)))
  expired?)

(module+ test
  (require rackunit)

  (test-case "retry sequence follows expected timeouts"
    (define policy (retry-policy 3 1000 0)) ; no jitter for predictable tests
    (define now 5000)

    ; First attempt
    (define state1 (start-retry policy now))
    (check-equal? (retry-state-attempt-num state1) 1)
    (check-equal? (retry-state-deadline state1) (+ now 1000))
    (check-false (expired? state1 (+ now 999)))
    (check-true (expired? state1 (+ now 1000)))

    ; Second attempt (2x timeout)
    (define state2 (next-retry state1 (+ now 1000)))
    (check-equal? (retry-state-attempt-num state2) 2)
    (check-equal? (retry-state-deadline state2) (+ now 1000 2000))

    ; Third attempt (4x timeout)
    (define state3 (next-retry state2 (+ now 3000)))
    (check-equal? (retry-state-attempt-num state3) 3)
    (check-equal? (retry-state-deadline state3) (+ now 3000 4000))

    ; No more attempts
    (check-false (next-retry state3 (+ now 7000))))

  (test-case "jitter affects deadlines"
    (define policy (retry-policy 2 1000 200))
    (define state (start-retry policy 1000))

    ; Deadline should be base +/- jitter/2
    (define deadline (- (retry-state-deadline state) 1000))
    (check-true (<= 900 deadline 1100) (format "deadline ~a not within expected range" deadline)))

  (test-case "expired? returns #f until deadline"
    (define policy (retry-policy 2 1000 0))
    (define state (start-retry policy 1000))

    (check-false (expired? state 1000))
    (check-false (expired? state 1999))
    (check-true (expired? state 2000))
    (check-true (expired? state 2001))))
