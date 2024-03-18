#lang racket/base

(require "message.rkt")
(require "client.rkt")

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included LICENSE-MIT and LICENSE-APACHE files.
;; If you would prefer to use a different license, replace those files with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here



(module+ test
  )

(module+ main
  (require racket/cmdline)
  #|(define who (box "world"))
  (command-line
   #:program "my-program"
   #:once-each
   [("-n" "--name") name "Who to say hello to" (set-box! who name)]
   #:args ()
   (printf "hello ~a~n" (unbox who)))|#

  ; basic DHCPDISCOVER transmission

  ; how do we set up the test harness per unit test and then teardown?
  ; Would like to:
  ;  - create a network namespace (needs sudo)
  ;  - spawn the actual unit test racket code as a racket process within the namespace
  ;  - possibly run dnsmasq at some point
  ;  - then remove the namespace on exit.
  ; raco test and other things will however look for all .rkt files and run their test submodule if found
  ; one option is to start the whole test suite in the network namespace, and then use individual dnsmasq
  ; executions per test.
  ; also use dnsmasq pcap trace capabilities
  ; wireshark tracing
  ;
  ; would be nice to be able to test state transitions, which implies a model where we can control when the
  ; state machine ticks. i.e. even if we want to replay packets, do so in a controlled manner.
  ; tcpreplay may allow replaying actual packets.
  ; however what I mean is, even when reading dnsmasq responses from the network, separate the packet reading
  ; so we can force the state machine to tick at our own pace.
  (define client (make-dhcp-client))
  (run client))
