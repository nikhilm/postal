#lang racket/base

(require racket/udp)
(require "message.rkt")

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

(struct dhcp-client
  (state))

(define (make-dhcp-client)
  (dhcp-client 'init))

#|the DHCPREQUEST message MUST use the same
     value in the DHCP message header's 'secs' field and be sent to the
     same IP broadcast address as the original DHCPDISCOVER message.|#

(define (run _client)
  ; TODO: Handle break to potentially shutdown cleanly (i.e. give up lease)
  ;(let loop ())

  ; TODO: Decide whether to start in 'init or 'init-reboot
  (define sock (udp-open-socket))
  (udp-bind! sock #f 68 #t)
  (udp-send-to sock "255.255.255.255" 67 (make-dhcpdiscover)))

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

; for easier rapid iteration.
; but probably needs sudo.
; would be nice to set up a user namespace + corresponding dhcp server within which to practice.
(module+ main
  (define client (make-dhcp-client))
  (run client))