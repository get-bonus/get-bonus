#lang racket/base

(module+ main
  (require racket/match)

  (for ([orig-res
         (in-list
          '("NES"
            (256 . 240)
            (256 . 224)
            "SNES"
            (256 . 224)
            ;;(512 . 224)
            (256 . 239)
            ;;(512 . 239)
            ;;(512 . 448)
            ;;(512 . 478)
            "GBA"
            (240 . 160)
            "DS"
            (256 . 192)
            "PSP"
            (480 . 272)
            "iPhone"
            (320 . 480)
            (640 . 960)
            "Vita"
            (960 . 544)
            "HD"
            (1280 . 720)
            (1920 . 1080)))])
    (match orig-res
      [(? string? s)
       (displayln s)]
      [(cons width height)
       (define the-i
         (for/or ([i (in-naturals)])
           (and (width . <= . (* 16 i))
                (height . <= . (* 9 i))
                i)))

       (define wide-res
         (cons (* 16 the-i)
               (* 9 the-i)))       

       (printf "~a-> 16:9*~a -> ~a\n"
               orig-res
               the-i
               wide-res)])))

