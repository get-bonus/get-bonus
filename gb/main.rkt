#lang racket/base
(require (for-syntax racket/base))

(begin-for-syntax
  (require racket/runtime-path)
  (define-runtime-path games "../games"))
