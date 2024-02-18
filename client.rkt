#lang racket/base

(require racket/udp)
(require "message.rkt")

(provide make-dhcp-client
         run)

; perhaps make these stateful enums

(struct dhcp-client
  (state))

(define (make-dhcp-client)
  (dhcp-client 'init))

(define (run _client)
  ; TODO: Handle break to potentially shutdown cleanly (i.e. give up lease)
  ;(let loop ())
  (define sock (udp-open-socket))
  (udp-bind! sock #f 68 #t)
  (udp-send-to sock "255.255.255.255" 67 (make-dhcpdiscover)))

; for easier rapid iteration.
; but probably needs sudo.
; would be nice to set up a user namespace + corresponding dhcp server within which to practice.
(module+ main
  (define client (make-dhcp-client))
  (run client))