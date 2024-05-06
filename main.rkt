#lang racket/base

(require "client.rkt")

(module+ main
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
