#!/usr/bin/env bash
strace -f -e trace=network -s 100 skypeforlinux 2>&1  | rg -N 'sendto\((\d+),[^,]+,\s+(\d+)' --only-matching  -r '$1,$2'
