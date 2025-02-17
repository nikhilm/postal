#lang racket/base

(require racket/contract)
(require racket/match)
(require "logger.rkt")

(provide
 ; Create a retry policy with max attempts, base timeout (ms), and jitter range (ms)
 (struct-out retry-policy)

 ; Start a new retry sequence
 start-retry

 ; Check if current attempt has expired
 expired?

 ; Get next retry state, or #f if exhausted
 next-retry

 ; Get deadline for current attempt
 (rename-out [retry-state-deadline get-deadline]))

; Configuration for retry behavior
(struct retry-policy (max-attempts      ; Maximum number of attempts
                      base-timeout       ; Base timeout in milliseconds
                      jitter-range) #:transparent)  ; +/- jitter range in milliseconds

; Current state of a retry operation
(struct retry-state (policy            ; The retry-policy being used
                     attempt-num        ; Current attempt number (1-based)
                     deadline) #:transparent)    ; When current attempt expires

; Start a new retry sequence
(define/contract (start-retry policy now)
  (retry-policy? exact-nonnegative-integer? . -> . retry-state?)
  (define timeout (calculate-timeout policy 1))
  (log-postal-debug "Starting new retry sequence with policy ~a at ~a" policy now)
  (retry-state policy 1 (+ now timeout)))

(define (calculate-timeout policy attempt-num)
  (define base-timeout (retry-policy-base-timeout policy))
  (define jitter (- (random (retry-policy-jitter-range policy))
                    (quotient (retry-policy-jitter-range policy) 2)))
  (define timeout (+ (* base-timeout (expt 2 (sub1 attempt-num)))
                     jitter))
  (log-postal-debug "Calculated timeout for attempt ~a: base=~ams, jitter=~ams, total=~ams"
                    attempt-num base-timeout jitter timeout)
  timeout)

(define/contract (next-retry state now)
  (retry-state? exact-nonnegative-integer? . -> . (or/c #f retry-state?))
  (define policy (retry-state-policy state))
  (define next-attempt (add1 (retry-state-attempt-num state)))

  (if (> next-attempt (retry-policy-max-attempts policy))
      (begin
        (log-postal-debug "No more retries available after attempt ~a" (retry-state-attempt-num state))
        #f)  ; No more retries
      (let ([next-deadline (+ now (calculate-timeout policy next-attempt))])
        (log-postal-debug "Moving to retry attempt ~a with next deadline ~a" next-attempt next-deadline)
        (retry-state policy
                     next-attempt
                     next-deadline))))

(define/contract (expired? state now)
  (retry-state? exact-nonnegative-integer? . -> . boolean?)
  (define expired? (>= now (retry-state-deadline state)))
  (when expired?
    (log-postal-debug "Retry timeout expired at ~a (deadline was ~a)"
                      now (retry-state-deadline state)))
  expired?)
