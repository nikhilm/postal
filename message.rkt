#lang racket/base

(require racket/port)
(require binaryio)

(provide make-dhcpdiscover)

(define (make-dhcpdiscover)
  (with-output-to-bytes
    (lambda ()
      ; bootp op
      ; BOOTREQUEST
      (write-byte 1)

      ; htype - 10mb ethernet. i see no reason to use another.
      (write-byte 1)

      ; hlen
      (write-byte 6)

      ; hops - client sets to zero
      (write-byte 0)

      ; xid
      ; TODO: Pick sequential random numbers
      (write-integer (random 4294967087) 4 #f)

      ; secs
      ; TODO: Better
      (write-integer 5 2 #f)

      ; flags should be set to 1
      (write-integer 1 2 #f)

      ; ciaddr
      (write-integer 0 4 #f)

      ; yiaddr
      (write-integer 0 4 #f)

      ; siaddr
      (write-integer 0 4 #f)

      ; giaddr
      (write-integer 0 4 #f)

      ; chaddr
      ; currently the mac address for this computer's wifi
      ; 70:cd:0d:a0:4d:d5
      ; this is 6 bytes, with another 10 bytes of padding.
      ; written as straight byte at a time, no endianness
      (write-bytes #"\x70\xcd\x0d\xa0\x4d\xd5\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00")

      ; sname
      (write-bytes (make-bytes 64))

      ; file
      (write-bytes (make-bytes 128))

      ; options
      ;; The first four octets of the 'options' field of the DHCP message
      ;; contain the (decimal) values 99, 130, 83 and 99, respectively (this
      ;; is the same magic cookie as is defined in RFC 1497 [17]
      (write-bytes (bytes 99 130 83 99))

      ; dhcp message type
      (write-bytes (bytes 53 1 1))

      ; TODO: dhclient writes hostname, consider doing that.

      ; end option. not sure if this is required.
      (write-byte 255))))

#|
sketch is that we want to feed data to the parser, and all down the parse stream, it should
yield back if it gets a meaningful packet or not. based on taht we want to keep feeding it more stuff
so something like

; writer
(define parser (make-parser))
(udp-receive! socket bs)
(for ([message (in-producer parser bs)
|#