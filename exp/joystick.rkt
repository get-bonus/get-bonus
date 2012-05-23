#lang racket/base
(require "system-select.rkt")

(system-select
 [(unix) "joystick-unix.rkt"]
 [(macosx) "joystick-osx.rkt"])
