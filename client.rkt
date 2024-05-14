#lang racket/base

(require racket/match)
(require racket/function)
(require racket/udp)
(require racket/list)
(require "message.rkt")
(require "state-machine.rkt")

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
       (define msg (parse (subbytes resp 0 n)))
       (eprintf "GOT INCOMING UDP MESSAGE ~v from ~v~n" msg src)
       (channel-put ch (incoming src msg))
       (loop)))))

; this struct does not have any state right now, but will likely need config options and stuff eventually.
(struct dhcp-client ())
(define (make-dhcp-client) (dhcp-client))

; TODO
#|the DHCPREQUEST message MUST use the same
     value in the DHCP message header's 'secs' field and be sent to the
     same IP broadcast address as the original DHCPDISCOVER message.|#

(define (run _client)
  ; TODO: Handle break to potentially shutdown cleanly (i.e. give up lease)
  ;(let loop ())

  ; TODO: Decide whether to start in 'init or 'init-reboot
  ; TODO: The protocol assumes packets are reliably delivered.
  ; Should SELECTING fall back to INIT if no packets arrive
  ; for example, with some cap on retries.
  (define sock (udp-open-socket))
  (udp-bind! sock #f 68 #t)

  (define ch (make-channel))
  ; TODO: Custodians and things for shutting down run
  (define recv-thread (make-recv-thread sock ch))
  (let loop ([sm (make-state-machine)]
             ; start with an immediately triggered alarm to get things going.
             [alarm (alarm-evt 0 #f)]
             [packets-to-send null])
    (define (spin msgs)
      (match-define (update sm2 next-instant outgoing) (step sm (current-inexact-monotonic-milliseconds) msgs))
      (loop sm2 (alarm-evt next-instant #t) (append packets-to-send outgoing)))

    (eprintf "SPIN LOOP ~v ~v~n" sm packets-to-send)
    (sync
     (handle-evt alarm (thunk* (spin null)))
     (handle-evt ch (compose1 spin list))
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
                                       (loop sm alarm (rest packets-to-send)))))))))))


(define (pick-recepient addr)
  (case addr
    [(broadcast) "255.255.255.255"]
    [else addr]))

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
10. Gugu
11. Eventually perform ARP scan, although this may require raw sockets
|#

(module+ test
  (require rackunit)
  (test-case "Malformed incoming messages are discarded"
             (fail "TODO: Implement me")))

