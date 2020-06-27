#!/bin/sh

INITTY=/dev/tty[1-6]
for tty in $INITTY; do
    setleds -D +num < $tty
done
