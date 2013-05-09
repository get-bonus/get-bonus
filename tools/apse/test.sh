#!/bin/sh

rm -fr db
rkt convert.rkt -- db ~/Downloads/anki-monster-pngs/*png
rkt display.rkt -- db 105 test.png
