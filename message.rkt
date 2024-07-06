#lang racket/base

(require racket/contract)
(require racket/match)
(require racket/string)
; silly, but required for hex-string->bytes.
(require file/sha1)
(require racket/port)
(require racket/pretty)
; TODO: If we are only using write-integer and read-integer, we can inline their suggested
; definition from the doc and remove this dependency.
(require binaryio)
(require net/ip)

(provide make-dhcpdiscover
         encode
         parse
         (struct-out message)
         (struct-out message-option)
         optionsf
         interface-mac-addr
         make-mac-addr)

(struct message-option (tag value) #:transparent)

(define (write-addr-option tag value)
  (write-byte tag)
  (write-byte 4)
  (write-int-addr (ip-address->number value)))

(define/match (write-option option)
  [((message-option 50 value)) (write-addr-option 50 value)]
  [((message-option 54 value)) (write-addr-option 54 value)])

(struct message
  (type ; symbol
   xid ; int
   secs ; int
   ciaddr ; ip-address?
   yiaddr ; ip-address?
   siaddr ; ip-address?
   giaddr ; ip-address?
   options ; list
   ) #:transparent)

; TODO: Move out of this module.
(define interface-mac-addr
  (make-parameter
   #f
   (lambda (v)
     (unless (equal? (bytes-length v) 16)
       (error "Must be a 16-byte byte string"))
     v)))

; Convert a string mac addr of the form "xx:xx:xx:xx:xx:xx" to a reasonable byte string to be used with interface-mac-addr
(define (make-mac-addr in)
  (let* ([plain (string-replace in ":" "")]
         [converted (hex-string->bytes plain)])
    (unless (equal? (bytes-length converted) 6)
      (error "Invalid mac address: ~v" in))
    (bytes-append converted (make-bytes 10 0))))

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
  (number->ipv4-address (read-integer 4 #f)))

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
      (write-int-addr (ip-address->number (message-ciaddr msg)))

      ; yiaddr
      (write-int-addr (ip-address->number (message-yiaddr msg)))

      ; siaddr
      (write-int-addr (ip-address->number (message-siaddr msg)))

      ; giaddr
      (write-int-addr (ip-address->number (message-giaddr msg)))

      ; chaddr
      (let ([mac (interface-mac-addr)])
        (unless mac
          (error "parameter interface-mac-addr is not set!"))
        (write-bytes mac))

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

(define (read-len-prefixed-time)
  (unless (eq? 4 (read-byte))
    (error "Expected time-like option to be exactly 4 bytes"))
  (read-integer 4 #f))

(define (read-len-prefixed-addr)
  (unless (eq? 4 (read-byte))
    (error "Expected addr-like option to be exactly 4 bytes"))
  (make-ip-address (read-bytes 4)))

(define (read-addr-list)
  (let ([len (read-byte)])
    (unless (= 0 (remainder len 4))
      (error "Expected a multiple of 4 for the length, but got ~v" len))
    (for/list ([_ (in-range 0 (quotient len 4))])
      (make-ip-address (read-bytes 4)))))

(define (read-vendor-extension)
  (match (read-byte)
    [0 (values 'pad #f)]
    [255 (values 'end #f)]
    [1 (values 'subnet-mask (read-len-prefixed-addr))]
    [3 (values 'router (read-addr-list))]
    [6 (values 'dns-server (read-addr-list))]
    [28 (values 'broadcast-address (read-len-prefixed-addr))]
    [51 (values 'lease-time (read-len-prefixed-time))]
    [54 (values 'server-identifier (read-len-prefixed-addr))]
    [58 (values 'renewal-time (read-len-prefixed-time))]
    [59 (values 'rebinding-time (read-len-prefixed-time))]
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
      (define options
        (for/list ([(tag value) (in-producer read-vendor-extension)]
                   #:break (eq? tag 'end))
          (message-option tag value)))
      ; TODO: Is it valid for a message to have data beyond the last option?
      (if msg-type
          (message (int->dhcp-type (bytes->integer msg-type #f))
                   xid secs ciaddr yiaddr siaddr giaddr options)
          (error 'parse "No dhcp message type found")))))


; findf for message options
; returns the value or #f
(define (optionsf msg tag)
  (let ([opt (findf (lambda (opt) (eq? (message-option-tag opt) tag)) (message-options msg))])
    (and opt (message-option-value opt))))

(define (make-dhcpdiscover xid)
  (message 'discover
           xid
           0
           (number->ipv4-address 0)
           (number->ipv4-address 0)
           (number->ipv4-address 0)
           (number->ipv4-address 0)
           null))

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
