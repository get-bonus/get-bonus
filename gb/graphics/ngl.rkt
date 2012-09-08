#lang typed/racket/base
(require racket/match)

;; A "modern" OpenGL 2D graphics engine.

;; Features
;;  - Relies on a texture atlas
;;  - Only allows rectangular sprites
;;  - Multi-pass shading

