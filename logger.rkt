#lang racket/base
(provide log-postal-debug
         log-postal-error
         log-postal-fatal
         log-postal-info
         log-postal-warning)

(define-logger postal)