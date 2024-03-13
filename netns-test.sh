#!/bin/bash
set -uxo pipefail

# Create network namespace
ip netns delete dhcp-test-server 2>/dev/null || true
ip netns delete dhcp-test-client 2>/dev/null || true
ip netns add dhcp-test-server || true
ip netns add dhcp-test-client || true

# Set up virtual interfaces
ip link add veth0 type veth peer name veth1
ip link set veth0 netns dhcp-test-server
ip link set veth1 netns dhcp-test-client

# Configure namespace
ip netns exec dhcp-test-server ip addr add 192.168.1.1/24 dev veth0
ip netns exec dhcp-test-server ip link set veth0 up
# TODO: Figure out how to kill these
# tail /var/log/syslog to see these messages.
# Can also always launch wireshark within the network namespace.
ip netns exec dhcp-test-server dnsmasq --dhcp-range=192.168.1.100,192.168.1.200 --interface=veth0 --dhcp-leasefile=/dev/null --log-queries=extra --log-debug

sleep 1

# Run DHCP client in namespace
# TODO: Clean up path
# ip netns exec dhcp-test-client ip addr add 192.168.1.2/24 dev veth1
# TODO: If we cna express each test as running
# as a separate IP or something, then we may
# not need to spin up dnsmasq every time.
ip netns exec dhcp-test-client ip link set veth1 up
ip netns exec dhcp-test-client ip route add default dev veth1
# ip netns exec dhcp-test-client /bin/bash
# there is flakiness sometimes and this won't get packets.
/home/nikhil/racket-8.12/bin/racket -A /home/nikhil/.local/share/racket/ main.rkt

# Clean up
ip netns delete dhcp-test-server 2>/dev/null || true
ip netns delete dhcp-test-client 2>/dev/null || true
pkill -f interface=veth0
