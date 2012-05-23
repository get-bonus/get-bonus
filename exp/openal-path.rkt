#lang racket/base
(require "system-select.rkt")

(system-select
 [(unix) "openal-path-unix.rkt"]
 [(macosx) "openal-path-osx.rkt"])
