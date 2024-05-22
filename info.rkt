#lang info
(define collection "postal")
(define deps '("base" "binaryio-lib" "net-ip-lib"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/postal.scrbl" ())))
(define pkg-desc "A DHCP client")
(define version "0.1")
(define pkg-authors '(racket-packages@me.nikhilism.com))
(define license 'MIT)

; TBD
;(define test-command-line-arguments
; '(("")))
