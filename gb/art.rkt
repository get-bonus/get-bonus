#lang racket/base
(require mode-lambda
         gfx/color
         racket/match
         apse)

(define sd (make-sprite-db))

;; SNES is 256x224, 26 = 416x234, 24 = 384x216, 32 = 512x288
;;
;; 24 is nice because it is divisible by 8, a common sprite size, but
;; is technically smaller on the Y axis
(define GB-SNES-SCALE (* 3 8))
(define W (* GB-SNES-SCALE 16))
(define H (* GB-SNES-SCALE 9))

;; NES had about 12 colors, plus some tones/shades
(define cw-slots 12)
(define cw:hi (color-wheel cw-slots))
(define cw:med (color-wheel cw-slots #:s 0.67 #:b 0.6))

;; xxx change -idxs to return all rotations
(for ([ccc (in-list
            #;(split-complementary-idxs cw-slots)
            #;(triadic-idxs cw-slots)
            (analogous-idxs cw-slots))]
      [i (in-naturals)])
  (define id (string->symbol (format "pal:ana:~a" i)))
  (printf "pal ~v\n" id)
  (match-define (vector lefti middlei righti) ccc)
  (define left (list-ref cw:hi lefti))
  (define middle (list-ref cw:hi middlei))
  (define right (list-ref cw:hi righti))
  (add-palette! sd id
                (apse-palette left middle right
                              (list-ref cw:med 0)
                              (list-ref cw:med 1)
                              (list-ref cw:med 2)
                              (list-ref cw:med 3))))

(define color-schemes (polygon-idxs 7 cw-slots))
(define (add-cw! CW fmt)
  (for ([c (in-list CW)]
        [i (in-naturals)])
    (define n (string->symbol (format fmt i)))
    (add-palette! sd n (color->palette c))))
(add-palette! sd 'grayscale (color->palette GRAY))
(add-cw! cw:hi "hi~a")
(add-cw! cw:med "med~a")
(add-apse-palette! sd)

;; xxx make the apse palette (3 tone + 4 hilight) wheel

;; xxx make some templates of different sizes

;; Some blocks
(define-sprite sd spr:block0
  $$$$$$$$
  $qqqqqq$
  $zaaaaq$
  $zaaaaq$
  $zaaaaq$
  $zaaaaq$
  $zzzzzq$
  $$$$$$$$)
(define-sprite sd spr:block1
  $$$$$$$$
  $qzzzzz$
  $zqqqqz$
  $zqqqqz$
  $zqqqqz$
  $zqqqqz$
  $zzzzzz$
  $$$$$$$$)
(define-sprite sd spr:block2
  $$$$$$$$
  $qzzzzz$
  $zqqzzz$
  $zqzzzz$
  $zzzzzz$
  $zzzzzz$
  $zzzzzz$
  $$$$$$$$)

;; Megaman
(define-sprite sd spr:megaman
  ________________________
  __________ggg___________
  ________gggqqg__________
  _______gzzzgqqg_________
  ______gzzzzzgggg________
  ______gzzzzzgqqzg_______
  _____gqzzzzzzggzg_______
  _____gqzzf!!!zz!g_______
  _____gqzf!!$$f$!g_______
  ______gzf!!$$f$!g_______
  _____ggzff!!!f!fg_______
  ___ggqqgzf$$$$fggg______
  __gzqqqqgfffffgqqzg_____
  __gzzqqqqgggggqqzzg_____
  _gzzzqgqqqqqqqgqzzzg____
  _gzzgggqqqqqqqgggzzg____
  _gzzzggqqqqqqqggzzzg____
  _gzzzggzzzzzzzggzzzg____
  __ggg_gzzzzzzzg_ggg_____
  _____gqqzzzzzqqg________
  ____gqqqqzzzqqqqg_______
  ___ggzqqqqgqqqzzgg______
  _ggzzzzzqg_gqzzzzzgg____
  gzzzzzzzg___gzzzzzzzg___
  ggggggggg___ggggggggg___)

(define-sprite sd spr:megaman1
  ________________________
  __________ggg___________
  ________gggddg__________
  _______gaaagddg_________
  ______gaaaaagggg________
  ______gaaaaagddag_______
  _____gdaaaaaaggag_______
  _____gdaaf!!!aa!g_______
  _____gdaf!!$$f$!g_______
  ______gaf!!$$f$!g_______
  _____ggaff!!!f!fg_______
  ___ggddgaf$$$$fggg______
  __gaddddgfffffgddag_____
  __gaaddddgggggddaag_____
  _gaaadgdddddddgdaaag____
  _gaagggdddddddgggaag____
  _gaaaggdddddddggaaag____
  _gaaaggaaaaaaaggaaag____
  __ggg_gaaaaaaag_ggg_____
  _____gddaaaaaddg________
  ____gddddaaaddddg_______
  ___ggaddddgdddaagg______
  _ggaaaaadg_gdaaaaagg____
  gaaaaaaag___gaaaaaaag___
  ggggggggg___ggggggggg___)

(define-sprite sd spr:megaman2
  ________________________
  __________ggg___________
  ________gggddg__________
  _______gaaqgddg_________
  ______gaaaqqgggg________
  ______gaaaaqgddag_______
  _____gdaaaaaqggqg_______
  _____gdaaf!!!qq!g_______
  _____gdzf!!$$f$!g_______
  ______gzf!!$$f$!g_______
  _____ggzff!!!f!fg_______
  ___ggddgzf$$$$fggg______
  __gaddddgfffffgddqg_____
  __gaaddddgggggddaqg_____
  _gaaadgdddddddgdaaqg____
  _gzagggdddddddgggaqg____
  _gzaaggdddddddggaaag____
  _gzaaggaaaaaaaggzaag____
  __ggg_gzaaaaaag_ggg_____
  _____gdezaaaadeg________
  ____gdddezaadddeg_______
  ___ggzdddegdddaqgg______
  _ggzaaaadg_gdaaaqqgg____
  gzzaaaaag___gzzaaaqqg___
  ggggggggg___ggggggggg___)

(module+ apse
  (with-apse-params [sd W H]
    (apse-sprite spr:megaman2 'pal:ana:6)))
