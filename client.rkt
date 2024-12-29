#lang racket/base

(require racket/contract)
(require racket/match)
(require racket/function)
(require racket/udp)
(require racket/list)
(require racket/port)
(require racket/pretty)
(require racket/string)
(require racket/system)

(require net/ip)

(require "message.rkt")
(require "state-machine.rkt")
(require "logger.rkt")

(provide make-dhcp-client
         run)

; perhaps make these stateful enums

; client states and exits (to guide the loop)
; init - exited by sending dhcpdiscover (so client initiated)
; selecting - exited after collecting enough DHCPOFFER and picking one
; requesting - exited on DHCPACK or DHCPNAK - new DHCPOFFER should be discarded.
; bound - exited on timer expiry
; renewing - exited on T2 expiry or DHCPNAK
; rebinding - exited on DHCPNAK
; init-reboot - exited by sending DHCPREQUEST
; rebooting - exited by DHCPACK or DHCPNAK

; this means, in the loop, for every state, we gotta sync on new messages or timer expiries of various kinds, or shutdown requests
; and then make a decision.

(define (parse-failure-handler data src e)
  (log-postal-warning #<<EOF
Dropped incoming message, since it was malformed.
  data length: ~a
  sender: ~a
  parse error: ~a
EOF
                      (bytes-length data) src (exn-message e)))

(define (parse-and-send-dgram data src ch)
  (with-handlers ([exn:fail:parse-message?
                   (lambda (e) (parse-failure-handler data src e))])
    (define msg (parse data))
    (log-postal-debug "Incoming message: ~a from ~v" (pretty-format msg) src)
    (channel-put ch (incoming (current-inexact-monotonic-milliseconds) src msg))))

(define (make-recv-thread sock ch)
  ; note that with an unbuffered channel
  ; we are relying on the caller reading messages
  ; from this thread fast enough that we aren't dropping
  ; udp messages due to full kernel buffers.
  ; TODO: a way to shut this thread down.
  (thread
   (lambda ()
     (define resp (make-bytes 65536))
     (let loop ()
       (define-values (n src _) (udp-receive! sock resp))
       (parse-and-send-dgram (subbytes resp 0 n) (make-ip-address src) ch)
       (loop)))))

; this struct does not have any state right now, but will likely need config options and stuff eventually.
(struct dhcp-client (mac-addr))
(define (make-dhcp-client mac-addr) (dhcp-client mac-addr))

; TODO
#|the DHCPREQUEST message MUST use the same
     value in the DHCP message header's 'secs' field and be sent to the
     same IP broadcast address as the original DHCPDISCOVER message.|#

(define (run client)
  ; TODO: Handle break to potentially shutdown cleanly (i.e. give up lease)
  ;(let loop ())

  ; TODO: Decide whether to start in 'init or 'init-reboot
  ; TODO: The protocol assumes packets are reliably delivered.
  ; Should SELECTING fall back to INIT if no packets arrive
  ; for example, with some cap on retries.
  (define sock (udp-open-socket #f 68))
  (udp-bind! sock #f 68 #t)

  (define ch (make-channel))
  ; TODO: Custodians and things for shutting down run
  (define recv-thread (make-recv-thread sock ch))
  (parameterize ([interface-mac-addr (make-mac-addr (dhcp-client-mac-addr client))])
    (let loop ([sm (make-state-machine)]
               ; start with an immediately triggered alarm to get things going.
               [alarm (alarm-evt 0 #f)]
               [packets-to-send null])
      (define/contract (spin incom)
        ((or/c incoming? #f) . -> . void)
        (define-values (next-wakeup-instant events)
          (sm (or incom (time-event (current-inexact-monotonic-milliseconds)))))
        (define-values (outgoing others) (partition send-msg? events))
        ; TODO: This is a bit of a bummer right now in running a blocking action.
        ; use async later.
        (for ([event (in-list others)])
          (when (iface-bind? event)
            (let ([client-addr (lease-info-client-addr (iface-bind-info event))])
              (log-postal-debug "Request to bind interface to ~a" client-addr)
              (set-ip-for-device "veth1" client-addr)))
          (when (iface-unbind? event)
            (error "TODO")))

        ; reconcile system state if required.
        (loop sm (alarm-evt next-wakeup-instant #t) (append packets-to-send outgoing)))

      (sync
       (handle-evt alarm (thunk* (spin #f)))
       (handle-evt ch spin)
       (if (null? packets-to-send)
           never-evt
           ; this is basically saying: when the socket is ready to receive data
           ; grab the first outgoing packet, and replace the event with an attempt
           ; to send that packet. then continue the loop.
           ; waiting on readiness allows delaying unpacking the first outgoing packet.
           (replace-evt (udp-send-ready-evt sock)
                        (thunk*
                         (match-let ([(send-msg msg to) (first packets-to-send)])
                           (handle-evt (udp-send-to-evt sock
                                                        (pick-recepient to)
                                                        67
                                                        (encode msg))
                                       (lambda (e)
                                         (log-postal-debug "Sent ~v to ~v" msg to)
                                         (loop sm alarm (rest packets-to-send))))))))))))


(define/match (pick-recepient addr)
  [(broadcast) "255.255.255.255"]
  [(_) (ip-address->string addr)])

; TODO: Switch to using CFFI
(define (ip-for-device dev)
  (define lines (with-input-from-file "/proc/net/arp" port->lines))
  (for/first ([line (in-list lines)]
              #:do [(define split (string-split line))]
              #:when (equal? (last split) dev))
    (first split)))

(define (set-ip-for-device dev ip)
  (let ([cmd (format "sudo ip addr add ~a/24 dev ~a" (ip-address->string ip) dev)])
    (log-postal-info "Running ~a" cmd)
    (system cmd)))

#|
OK, few things to sort out with the design
1. we shouldn't block the "main thread" on reading. In fact, we should be able to keep processing our state transitions loop while waiting for inputs, and if inputs come in, handle based on the state.
2. Multiple DHCPOFFERs may be received, and should all be read before doing stuff.
2. Should be able to handle breaks cleanly.
3. Transition states.
4. Way better test infra.
5. Be able to set the local IP address on linux
6. Send and parse various DHCP messages.
7. Ideally drop as many privileges as we can at the beginning, since we are running as root.
  7a. Consider using pledge - https://justine.lol/pledge/
8. Logging
9. Ideally make some of this nice and interactive... somehow.
11. Eventually perform ARP scan, although this may require raw sockets
|#

(module+ test
  (require rackunit)
  (test-case
   "Malformed incoming messages are discarded"
   (define correct
     (parameterize ([interface-mac-addr (make-mac-addr "00:11:22:33:44:55")])
       (encode (make-dhcpdiscover 17))))
   (define malformed (subbytes correct 5))
   (define ch (make-channel))
   (thread
    (lambda ()
      (parse-and-send-dgram malformed #f ch)))
   (sync (handle-evt ch
                     (lambda (_) (fail "Channel should not be ready, since message is discarded")))
         (system-idle-evt))))
