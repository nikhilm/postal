#!/bin/bash

# Create network namespace
ip netns add dhcp-test || true

# Set up virtual interfaces
ip link add veth0 type veth peer name veth1
ip link set veth0 netns dhcp-test

# Configure namespace
ip netns exec dhcp-test ip addr add 192.168.1.1/24 dev veth0
ip netns exec dhcp-test ip link set veth0 up
ip netns exec dhcp-test dnsmasq --dhcp-range=192.168.1.100,192.168.1.200 --interface=veth0 --dhcp-leasefile=/dev/null &

# Run DHCP client in namespace
ip netns exec dhcp-test /path/to/your/dhcp/client

# Clean up
ip netns delete dhcp-test 2>/dev/null || true
