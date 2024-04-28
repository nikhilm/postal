#lang racket/base

(require racket/match)
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
; should open _one_ UDP socket on port 68 and start a thread for reading.

(define (make-recv-thread sock ch)
  ; note that with an unbuffered channel
  ; we are relying on the caller reading messages
  ; from this thread fast enough that we aren't dropping
  ; udp messages due to full kernel buffers.
  ; TODO: a way to shut this thread down.
  (thread
   (lambda ()
     ; TODO: This is not a loop right now.
     ; TODO: Share this buffer across iterations.
     (define resp (make-bytes 65536))
     (define-values (n src src-port) (udp-receive! sock resp))
     (eprintf "GOT INCOMING UDP MESSAGE~n")
     (channel-put ch (incoming src (parse (subbytes resp 0 n)))))))

(struct dhcp-client
  (state))

(define (make-dhcp-client)
  (dhcp-client 'init))

; TODO
#|the DHCPREQUEST message MUST use the same
     value in the DHCP message header's 'secs' field and be sent to the
     same IP broadcast address as the original DHCPDISCOVER message.|#

(define (send-discover sock)
  ; TODO: xid
  (udp-send-to sock "255.255.255.255" 67 (encode (make-dhcpdiscover 456))))

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
  (define recv-thread (make-recv-thread sock ch))
  (define sm (make-state-machine))
  (match-define (update sm1 timeout-instant outgoing) (update-state sm (current-inexact-monotonic-milliseconds) null))
  (let loop ([sm sm1]
             [alarm never-evt]
             ; TODO: This implicitly relies on a transition happening during init
             [packets-to-send outgoing])
    (define (spin msgs)
      ; little confused about what to set the timeout here
      ; should it be the new timeout only, or the min of the current and the suggested
      ; seems like it should be the new timeout only
      (match-define (update sm2 next-instant outgoing) (update-state sm (current-inexact-monotonic-milliseconds) msgs))
      (eprintf "Next inst ~v now ~v~n" next-instant (current-inexact-monotonic-milliseconds))
      (loop sm2 (alarm-evt next-instant #t) (append packets-to-send outgoing)))
    (eprintf "SPIN LOOP ~v ~v~n" sm packets-to-send)
    (apply
     sync
     (handle-evt alarm
                 (lambda (e)
                         (printf "Aalarm~n")
                   (spin null)))
     ;(handle-evt next-timeout (lambda (e)
     ; (update-state sm (monotonic-time) null)))
     (handle-evt ch (lambda (incoming)
                      (eprintf "GOT ~v~n" incoming)
                      ; TODO: Err this will result in outgoing data or timeouts. need to handle
                      ; those will influence the set of evts to monitor on the next iteration.
                      (spin (list incoming))))
     (if (null? packets-to-send)
         null
         ; if i understand this correctly
         (list (replace-evt (udp-send-ready-evt sock)
                            (lambda (e)
                              (eprintf "YO UDP READY~n")
                              (match-let ([(send-msg msg to) (first packets-to-send)])
                                (handle-evt (udp-send-to-evt sock
                                                             (if (eq? 'broadcast to) "255.255.255.255" to)
                                                             67
                                                             (encode msg))
                                            (lambda (e)
                                              (loop sm alarm (rest packets-to-send))))))))))))
; TODO: Will actally look something like
;(sync evts)
; the initial thing will have nothing to pass to the input
; sync on:
; incoming packets on the channel
;   feed to the state machine, and process outgoing events
; outgoing send events when the socket is sendable
; next timer event when requested by the state-machine
; hmm, what happens when the state machine doesn't want packets, but is given packets?
; don't do that I guess, because the packets will be ignored.
; so perhaps buffer the messages.
; handle timeouts and retries for things like waiting for DHCPACK/NACK

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



