#lang racket/base

(require racket/match)
(require racket/port)
; TODO: If we are only using write-integer and read-integer, we can inline their suggested
; definition from the doc and remove this dependency.
(require binaryio)
(require racket/contract)

(struct message-option (tag value))

(define (write-addr-option tag value)
  (write-byte tag)
  (write-byte 4)
  (write-int-addr value))

(define/match (write-option option)
  [((message-option 50 value)) (write-addr-option 50 value)]
  [((message-option 54 value)) (write-addr-option 54 value)])

(struct message
  (type ; symbol
   xid ; int
   secs ; int
   ciaddr ; int
   yiaddr ; int
   siaddr ; int
   giaddr ; int
   options ; list
   ) #:transparent)

; TODO: Move out of this module.
(define (get-interface-mac-addr)
  ; chaddr
  ; currently the mac address for this computer's wifi
  ; 70:cd:0d:a0:4d:d5
  ; this is 6 bytes, with another 10 bytes of padding.
  ; written as straight byte at a time, no endianness
  ;#"\x70\xcd\x0d\xa0\x4d\xd5\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00")
  #"\xf6\x52\x8d\x05\xc0\x45\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00")

(define (dhcp-type->int type)
  (match type
    ['discover 1]
    ['offer 2]
    ['request 3]
    ['decline 4]
    ['ack 5]
    ['nak 6]
    ['release 7]
    ['inform 8]))

(define (int->dhcp-type n)
  (match n
    [1 'discover]
    [2 'offer]
    [3 'request]
    [4 'decline]
    [5 'ack]
    [6 'nak]
    [7 'release]
    [8 'inform]))

(define (write-int-addr addr)
  (write-integer addr 4 #f))

(define (read-int-addr)
  (read-integer 4 #f))

(define/contract (encode msg)
  (message? . -> . bytes?)
  (with-output-to-bytes
    (lambda ()
      ; bootp op
      ; BOOTREQUEST
      ; TODO: Pick based on DHCP message type.
      (write-byte 1)
      ; htype - 10mb ethernet. i see no reason to use another.
      (write-byte 1)

      ; hlen
      (write-byte 6)

      ; hops - client sets to zero
      (write-byte 0)

      ; xid
      ; TODO: Pick sequential random numbers
      (write-integer (message-xid msg) 4 #f)

      ; secs
      (write-integer 0 2 #f)

      ; flags should set the most significant bit to 1 to request broadcast.
      (write-integer #x8000 2 #f)

      ; ciaddr
      (write-int-addr (message-ciaddr msg))

      ; yiaddr
      (write-int-addr (message-yiaddr msg))

      ; siaddr
      (write-int-addr (message-siaddr msg))

      ; giaddr
      (write-int-addr (message-giaddr msg))

      ; chaddr
      (write-bytes (get-interface-mac-addr))

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
      (write-bytes (bytes 53 1 (dhcp-type->int (message-type msg))))

      (for ([option (in-list (message-options msg))])
        (write-option option))
      (write-bytes (bytes 255)))))

(define (read-vendor-extension)
  (match (read-byte)
    [0 (values 'pad #f)]
    [255 (values 'end #f)]
    ; TODO: Handle receiving less than n bytes.
    ; seems like that should be an error.
    [tag (let ([n (read-byte)])
           (values tag (read-bytes n)))]))

(define/contract (parse buf)
  (bytes? . -> . message?)
  (with-input-from-bytes buf
    (lambda ()
      ; bootp message type to discard
      ; TODO: Perhaps validate against the dhcp type
      (read-byte)

      ; htype, don't care
      (read-byte)

      ; hlen, don't care right now
      ; TODO: pass on server mac
      (read-byte)

      ; hops - don't care
      (read-byte)

      (define xid (read-integer 4 #f))
      (define secs (read-integer 2 #f))
      ;flags
      (read-integer 2 #f)
      ; ciaddr
      (define ciaddr (read-int-addr))
      (define yiaddr (read-int-addr))
      (define siaddr (read-int-addr))
      (define giaddr (read-int-addr))
      (define chaddr (read-bytes 16))
      (define sname (read-bytes 64))
      (define file (read-bytes 128))
      ; magic
      (match (read-bytes 4)
        [#"c\202Sc" void])
      (define msg-type
        (for/first ([(tag value) (in-producer read-vendor-extension)]
                    #:break (eq? tag 'end)
                    #:when (eq? tag 53))
          value))
      (if msg-type
          (message (int->dhcp-type (bytes->integer msg-type #f))
                   xid secs ciaddr yiaddr siaddr giaddr null)
          (error 'parse "No dhcp message type found")))))

(provide make-dhcpdiscover
         encode
         parse
         (struct-out message)
         (struct-out message-option)
         with-options)

(define (with-options msg options)
  (struct-copy message msg [options options]))

(define (make-dhcpdiscover xid)
  (message 'discover xid 0 0 0 0 0 null))


(module+ test
  ; TODO: Request claude to generate a fuzzer for the parsers
  )
#|
sketch is that we want to feed data to the parser, and all down the parse stream, it should
yield back if it gets a meaningful packet or not. based on taht we want to keep feeding it more stuff
so something like

; writer
(define parser (make-parser))
(udp-receive! socket bs)
(for ([message (in-producer parser bs)
|#
