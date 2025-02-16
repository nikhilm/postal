#lang racket/base

(require racket/contract)
(require racket/match)
(require "logger.rkt")

(provide (struct-out retry-policy)
         (struct-out retry-state)
         make-retry-state
         next-retry-timeout
         retry-expired?)

; Configuration for retry behavior
(struct retry-policy (max-attempts      ; Maximum number of attempts
                      base-timeout       ; Base timeout in milliseconds
                      jitter-range) #:transparent)  ; +/- jitter range in milliseconds

; Current state of a retry operation
(struct retry-state (policy            ; The retry-policy being used
                     attempt-num        ; Current attempt number (1-based)
                     last-attempt-time  ; Timestamp of last attempt
                     deadline) #:transparent)    ; When current attempt expires

(define/contract (make-retry-state policy now)
  (retry-policy? exact-nonnegative-integer? . -> . retry-state?)
  (define timeout (next-timeout policy 1))
  (log-postal-debug "Creating new retry state with policy ~a at time ~a, first timeout in ~ams for a deadline of ~a"
                    policy now timeout (+ now timeout))
  (retry-state policy 1 now (+ now timeout)))

(define (next-timeout policy attempt-num)
  (define base-timeout (retry-policy-base-timeout policy))
  (define jitter (- (random (retry-policy-jitter-range policy))
                    (quotient (retry-policy-jitter-range policy) 2)))
  (define timeout (+ (* base-timeout (expt 2 (sub1 attempt-num)))
                     jitter))
  (log-postal-debug "Calculated timeout for attempt ~a: base=~ams, jitter=~ams, total=~ams"
                    attempt-num base-timeout jitter timeout)
  timeout)

(define/contract (next-retry-timeout state now)
  (retry-state? exact-nonnegative-integer? . -> . (or/c #f retry-state?))
  (define policy (retry-state-policy state))
  (define next-attempt (add1 (retry-state-attempt-num state)))

  (if (> next-attempt (retry-policy-max-attempts policy))
      (begin
        (log-postal-debug "No more retries available after attempt ~a" (retry-state-attempt-num state))
        #f)  ; No more retries
      (let ([next-deadline (+ now (next-timeout policy next-attempt))])
        (log-postal-debug "Moving to retry attempt ~a with next deadline ~a" next-attempt next-deadline)
        (retry-state policy
                     next-attempt
                     now
                     next-deadline))))

(define/contract (retry-expired? state now)
  (retry-state? exact-nonnegative-integer? . -> . boolean?)
  (define expired? (>= now (retry-state-deadline state)))
  (when expired?
    (log-postal-debug "Retry timeout expired at ~a (deadline was ~a)"
                      now (retry-state-deadline state)))
  expired?)
