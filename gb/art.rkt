#lang racket/base
(require mode-lambda
         mode-lambda/static
         mode-lambda/color
         racket/match
         apse)

;; https://www.reddit.com/r/gamedev/comments/79a93x/50_bite_sized_pixel_art_tutorials_and_tips_by/

;; XXX artist-mode, artist-select-op-flood-fill, and C-a C-c C-f is awesome

(define sd (make-sprite-db))
(initialize-apse! sd)

;; SNES is 256x224, 26 = 416x234, 24 = 384x216, 32 = 512x288
;;
;; 24 is nice because it is divisible by 8, a common sprite size, but
;; is technically smaller on the Y axis
(define GB-SNES-SCALE (* 3 8))
(define W (* GB-SNES-SCALE 16))
(define H (* GB-SNES-SCALE 9))

;; NES had about 12 colors, plus some tones/shades, the SNES had 16
(define cw-slots 16)
(define cw:hi (color-wheel cw-slots))
(define cw:med (color-wheel cw-slots #:s 0.67 #:b 0.6))

;; xxx need to come up with a different way to do this
;; xxx change -idxs to return all rotations
(define cw:hi:pals
  (for/list ([ccc (in-list
                   (split-complementary-idxs cw-slots)
                   #;(triadic-idxs cw-slots)
                   #;(analogous-idxs cw-slots))]
             [i (in-naturals)])
    (define id (string->symbol (format "pal:ana:~a" i)))
    (match-define (vector lefti middlei righti) ccc)
    (define left (list-ref cw:hi lefti))
    (define middle (list-ref cw:hi middlei))
    (define hi (argb 255 255 228 172))
    (define right (list-ref cw:hi righti))
    (add-palette! sd id
                  (apse-palette left middle right hi hi hi hi))
    id))

(define color-schemes (polygon-idxs 7 cw-slots))
(define (add-cw! CW fmt)
  (for ([c (in-list CW)]
        [i (in-naturals)])
    (define n (string->symbol (format fmt i)))
    (add-palette! sd n (color->palette c))))
(add-cw! cw:hi "hi~a")
(add-cw! cw:med "med~a")

;; xxx make the apse palette (3 tone + 4 hilight) wheel

(define-syntax-rule (define-palette sd id colors)
  (begin (define id 'id)
         (add-palette! sd 'id colors)))

(define (single-color-palette BLACK)
  (list TRANSPARENT BLACK
        BLACK BLACK BLACK
        BLACK BLACK BLACK
        BLACK BLACK BLACK
        BLACK BLACK BLACK BLACK
        BLACK))

(define-palette sd pal:grayscale
  (color->palette GRAY))
(define-palette sd pal:black
  (single-color-palette BLACK))
(define-palette sd pal:white
  (single-color-palette WHITE))
(define-palette sd pal:red
  (single-color-palette (argb 255 255 0 0)))

(define (monotone-palette COLOR)
  (list TRANSPARENT BLACK
        COLOR COLOR COLOR
        COLOR COLOR COLOR
        COLOR COLOR COLOR
        COLOR COLOR COLOR COLOR
        WHITE))

(define-palette sd pal:mono:white
  (monotone-palette WHITE))
(define-palette sd pal:mono:black
  (monotone-palette BLACK))

;; 12x16: Little Mario (SMB1):
;; 16x32: Big Mario (SMB1), Simon (CV1)
;; 16x16: Link (Z1), Bomberman, Bub & Bob (Bubble Bobble), Pacman
;; 24x36: Bill (Contra)
;; 24x32: Alex (RCR)
;; 24x24: Mega Man
;; 32x36: Mega Man X

;; Some blocks
(define-sprite sd spr:block0
  #:w 8 #:h 8
  $$$$$$$$
  $qqqqqq$
  $zaaaaq$
  $zaaaaq$
  $zaaaaq$
  $zaaaaq$
  $zzzzzq$
  $$$$$$$$)
(define-sprite sd spr:block1
  #:w 8 #:h 8
  $$$$$$$$
  $qzzzzz$
  $zqqqqz$
  $zqqqqz$
  $zqqqqz$
  $zqqqqz$
  $zzzzzz$
  $$$$$$$$)
(define-sprite sd spr:block2
  #:w 8 #:h 8
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
  #:w 24 #:h 24
  __________$$$___________
  ________$$$qq$__________
  _______$zzz$qq$_________
  ______$zzzzz$$$$________
  ______$zzzzz$qqz$_______
  _____$qzzzzzz$$z$_______
  _____$qzzf!!!zz!$_______
  _____$qzf!!$$f$!$_______
  ______$zf!!$$f$!$_______
  _____$$zff!!!f!f$_______
  ___$$qq$zf$$$$f$$$______
  __$zqqqq$fffff$qqz$_____
  __$zzqqqq$$$$$qqzz$_____
  _$zzzq$qqqqqqq$qzzz$____
  _$zz$$$qqqqqqq$$$zz$____
  _$zzz$$qqqqqqq$$zzz$____
  _$zzz$$zzzzzzz$$zzz$____
  __$$$_$zzzzzzz$_$$$_____
  _____$qqzzzzzqq$________
  ____$qqqqzzzqqqq$_______
  ___$$zqqqq$qqqzz$$______
  _$$zzzzzq$_$qzzzzz$$____
  $zzzzzzz$___$zzzzzzz$___
  $$$$$$$$$___$$$$$$$$$___)

(define-sprite sd spr:megaman1
  #:w 24 #:h 24
  __________$$$___________
  ________$$$dd$__________
  _______$aaa$dd$_________
  ______$aaaaa$$$$________
  ______$aaaaa$dda$_______
  _____$daaaaaa$$a$_______
  _____$daaf!!!aa!$_______
  _____$daf!!$$f$!$_______
  ______$af!!$$f$!$_______
  _____$$aff!!!f!f$_______
  ___$$dd$af$$$$f$$$______
  __$adddd$fffff$dda$_____
  __$aadddd$$$$$ddaa$_____
  _$aaad$ddddddd$daaa$____
  _$aa$$$ddddddd$$$aa$____
  _$aaa$$ddddddd$$aaa$____
  _$aaa$$aaaaaaa$$aaa$____
  __$$$_$aaaaaaa$_$$$_____
  _____$ddaaaaadd$________
  ____$ddddaaadddd$_______
  ___$$adddd$dddaa$$______
  _$$aaaaad$_$daaaaa$$____
  $aaaaaaa$___$aaaaaaa$___
  $$$$$$$$$___$$$$$$$$$___)

(define-sprite sd spr:megaman2
  #:w 24 #:h 24
  __________$$$___________
  ________$$$dd$__________
  _______$aaq$dd$_________
  ______$aaaqq$$$$________
  ______$aaaaq$dda$_______
  _____$daaaaaq$$q$_______
  _____$daaf!!!qq!$_______
  _____$dzf!!$$f$!$_______
  ______$zf!!$$f$!$_______
  _____$$zff!!!f!f$_______
  ___$$dd$zf$$$$f$$$______
  __$adddd$fffff$ddq$_____
  __$aadddd$$$$$ddaq$_____
  _$aaad$ddddddd$daaq$____
  _$za$$$ddddddd$$$aq$____
  _$zaa$$ddddddd$$aaa$____
  _$zaa$$aaaaaaa$$zaa$____
  __$$$_$zaaaaaa$_$$$_____
  _____$dezaaaade$________
  ____$dddezaaddde$_______
  ___$$zddde$dddaq$$______
  _$$zaaaad$_$daaaqq$$____
  $zzaaaaa$___$zzaaaqq$___
  $$$$$$$$$___$$$$$$$$$___)

;; xxx build a list of things i should draw

;; Characters
;; TODO Frog - Frog - Carried by a Robot
;; TODO Peach - Cat (Princess Super Kitty) - Carried by an Octopus
;; TODO Hazel - Duck - Riding a Hippo
;;
;; Sizes: Mario, Megaman, Link, Samus, Simon Belmont, Secret of Mana,
;; Pacman, Bomberman, Bubble Bobble, Mega Man X, River City Ransom
;;
;; Animations: Jumping (1 frame), Ducking (1 frame), Standing (1
;; frame), Walking Right (3 frames), Turning around (1 frame),
;; Climbing, Stairs (1 up, 1 down), Attack (3 frame), Shooting while
;; Running (3 frames)
;;
;; Each character also gets a vehicle for shooters and a kart for racing

(define-sprite sd spr:frog:16x16
  #:w 16 #:h 16
  ___$$$____$$$___
  __$!!!$__$!!!$__
  _$$!$!$$$$!$!$$_
  _$a!!!aaaa!!!a$_
  _$aaaaaaaaaaaa$_
  _$aaaaaaaaaaaa$_
  _$aaaaaaaaaaaa$_
  _$aaaa$aa$aaaa$_
  __$$aaaaaaaa$$__
  __$d$$aaaa$$d$__
  __$ddd$$$$ddd$__
  __$ddd$ss$ddd$__
  __$dddd$$dddd$__
  _$ddd$$$$$$ddd$_
  $ddd$______$ddd$
  _$$$________$$$_)

;; Attacks
;; TODO Fireball
;; TODO Bullet
;; TODO Weapons: Sword
;; TODO Weapons: Whip
;; TODO Bomb

;; Effects
;; TODO Explosions (see Retro Game Challenge)
;; TODO Lightning
;; TODO Hearts
(define-sprite sd spr:heart8
  #:w 8 #:h 8
  _$$__$$_
  $$a$$a$$
  $aaaaqa$
  $aaaaaa$
  $$aaaa$$
  _$$aa$$_
  __$$$$__
  ___$$___)

;; Libby thinks it looks like a praying mantis
(define-sprite sd spr:beat
  #:w 8 #:h 8
  _$$__$$_
  __$__$__
  $$$$$$$$
  $aaaaaa$
  $$zzzz$$
  _$$aa$$_
  __$$$$__
  ___$$___)
;; TODO Ice
;; TODO Flame

;; Fonts
;; TODO For scores
;; TODO For instructions

;; Puzzle
;; TODO Tetris Blocks (see above)
;; TODO Poyo Poyo Blobs
(define-sprite sd spr:puzzle:blob
  #:w 16 #:h 16
  _________$$$$$$_
  ________$$qqqq$_
  _______$$qq$$$$_
  ____$$$$qq$$____
  ___$$qaaaaa$$___
  __$$qaaaaaaz$$__
  _$$qaaaaaaaaz$$_
  $$qa!!aaaa!!az$$
  $aaa!$aaaa$!aaa$
  $aaaaaaaaaaaaaa$
  $aaaaaaaaaaaaaa$
  $aaa$$aaaa$$aaa$
  $aaaa$$$$$$aaaa$
  $aaaaaaaaaaaazz$
  $$aaaaaaaaaazz$$
  _$$$$$$$$$$$$$$_)
;; TODO Puzzle Fighter Gems
;; TODO Symbols for 10x8, Tetris Attack (5)
(define-sprite sd spr:puzzle:x
  #:w 16 #:h 16
  $$$$$______$$$$$
  $qqq$$____$$qqq$
  $qaaq$$__$$qaaq$
  $zaaaq$$$$qaaaz$
  $$zaaaaaaaaaaz$$
  _$$za!!aa!!az$$_
  __$$a!$aa$!a$$__
  ___$aaaaaaaa$___
  ___$aaaaaaaa$___
  __$$aaa$$aaa$$__
  _$$qaaaaaaaaq$$_
  $$qaaaaaaaaaaq$$
  $qaaaz$$$$zaaaq$
  $aaaz$$__$$zaaa$
  $aaz$$____$$zaa$
  $$$$$______$$$$$)
;; TODO Ball
(define-sprite sd spr:puzzle:ball8
  #:w 8 #:h 8
  __$$$$__
  _$$qq$$_
  $$aaqq$$
  $aaaaaa$
  $zaaaaa$
  $$zaaa$$
  _$$za$$_
  __$$$$__)
(define-sprite sd spr:puzzle:ball
  #:w 16 #:h 16
  _____$$$$$$_____
  ____$$aaqq$$____
  ___$$aaaaqq$$___
  __$$aaaaaaqq$$__
  _$$aaaaaaaaqq$$_
  $$aaaaaaaaaaqq$$
  $aaaa!!aa!!aaqq$
  $aaaa!!aa!!aaaa$
  $zaaa!$aa$!aaaa$
  $zzaaaaaaaaaaaa$
  $$zzaaaaaaaaaa$$
  _$$zzaa$$$$aa$$_
  __$$zzaaaaaa$$__
  ___$$zzaaaa$$___
  ____$$zzzz$$____
  _____$$$$$$_____)
;; TODO Stars
(define-sprite sd spr:puzzle:star
  #:w 16 #:h 16
  ______$$$$______
  _____$$qq$$_____
  _____$aaaq$_____
  ____$$aaaq$$____
  $$$$$aaaaaq$$$$$
  $aaaaaaaaaaqqqq$
  $zaaa$aaaa$aaaa$
  $$zaa$aaaa$aaz$$
  _$$za$aaaa$az$$_
  __$$aaaaaaaa$$__
  _$$$aaaaaaaa$$$_
  _$aaaaaaaaaaaq$_
  $$aaaz$$$zaaaq$$
  $aaaz$$$$$$zaaq$
  $zzz$$____$$zaq$
  $$$$$______$$$$$)
;; TODO Junk (Dr. Mario, etc)
(define-sprite sd spr:puzzle:ghost
  #:w 16 #:h 16
  ________________
  ________________
  ________________
  ________$$$$____
  _____$$$qaa$$___
  ____$$qaaa!!$___
  ___$$qaaaa!$$___
  __$$qaaaaa!!$___
  __$qaaaaaaaa$$$_
  __$aaaaazaaaaa$$
  _$aaaaaazzaaaaa$
  $aaaaaaaaaa$aaa$
  $$$aaaaaaaa$$az$
  __$$aaaaaaaa$$$$
  ___$za$$$$az$___
  ___$$$$__$$$$___)
;; TODO Coins (Money Puzzle Exchanger)

(define-sprite sd spr:puzzle:bomb-guy
  #:w 16 #:h 16
  _______$$$$$____
  _______$___$$___
  _______$$___$$__
  _____$$$$$$__$$_
  ____$$aaaq$$____
  ___$$aaaaaq$$___
  _$$$aaaaaaaq$$$_
  $$aaa!!aa!!aaq$$
  $a$aa!$aa!$aa$a$
  $a$aaaaaaaaaa$a$
  $$$aa$aaaa$aa$$$
  __$zaa$$$$aaa$__
  __$$zaaaaaaz$$__
  ___$$a$$$$a$$___
  ____$a$__$a$____
  ____$$$__$$$____)
(define-sprite sd spr:puzzle:bomb-guy:long-arms
  #:w 16 #:h 16
  _______$$$$$____
  _______$___$$___
  _______$$___$$aq
  _____$$$$$$__$$a
  ____$$aaaq$$__aa
  ___$$aaaaaq$$___
  _$$$aaaaaaaq$$$_
  $$qaa!!aa!!aaq$$
  $aaaa!$aa!$aaaq$
  $a$aaaaaaaaaa$a$
  $a$aa$aaaa$aa$a$
  $a$zaa$$$$aaa$a$
  $a$$$zaaaaz$$$a$
  $$$_$$$$$$$$_$$$
  ____$a$__$a$____
  ____$$$__$$$____)

(define-sprite sd spr:puzzle:elephant:0
  #:w 16 #:h 16
  ________________
  ________________
  _________$$_____
  ________$aq$$___
  _$_____$aaaaq$__
  $q$___$aaa!!a$__
  $aa$$$zaaa!$aq$_
  _$aaaazaaa!!aaq$
  __$aaaazaaaaaaa$
  __$aaaaaa$aaa$a$
  __$zaaaaaa$$$$a$
  ___$zaaaaaaaa$a$
  ___$zzzaaaz$$$a$
  ____$$$$$$$$_$a$
  _____$a$$a$__$$$
  _____$$$$$$_____)
(define-sprite sd spr:puzzle:elephant:1
  #:w 16 #:h 16
  ________________
  ________________
  _________$$_____
  ________$aq$$___
  __$____$aaaaq$__
  _$q$__$aaa!!a$__
  $aa$$$zaaa!$aq$_
  _$aaaazaaa!!aaq$
  __$aaaazaaaaaaa$
  __$aaaaaa$aaa$a$
  __$zaaaaaa$$$$a$
  ___$zaaaaaaaa$a$
  ___$zzzaaaz$$$a$
  ____$$$$$$$$_$$$
  ____$a$__$a$____
  ____$$$__$$$____)
(define ani:elephant:walking
  ;; Looks good at 5 FPS (incorporate that into the structure?)
  (animation (list spr:puzzle:elephant:0 spr:puzzle:elephant:1)))

;; SMB
;; TODO Coin
;; TODO Tree
;; TODO Block
;; TODO Pipe
;; TODO Turtle
;; TODO Wood block

;; Tiles (Zelda style)

;; Tiles (SMB style)
;; TODO Grass
;; TODO Rock
;; TODO Bubble Bobble

;; Tiles (Top-down)
;; TODO Walls w/ corners

;; Paletter #9 looks good for this
(define-sprite sd spr:tile:top-down:waves
  #:w 16 #:h 16
  aaaaaaaaaaaaaaaa
  aaazaaaaaaaaaaqa
  aaaazaaaaaaaaqaa
  aaaaaaaaaaaaaaaa
  aqaaaaaaazaaaaaa
  aaqaaaaaaazaaaaa
  aaaaazaaaaaaaaaa
  aaaaaazaaaaaaaaa
  aaaaaaaaaaqaaaaa
  aaaaaaaaaaaqaaaa
  aqaaaaaaaaaaaaaa
  aaqaaaaaaaaaazaa
  aaaaaaaaaaaazaaa
  aaaqaaaaaaaaaaaa
  aaqaaaaaaqaaaaaa
  aaaaaaaaqaaaaaaa)

(define-sprite sd spr:tile:top-down:grass
  #:w 16 #:h 16
  qzqqqzaqqaqqazaa
  qqzzaazqzzzqazzz
  zzazaqzqaaazazzq
  qazaaqqzazqazqqz
  aqaqaaqzzzazazqa
  qazqazaqaaqqazqq
  zaqaazqzzqzaaaqa
  azazzqzazazqqzqa
  qaqzaazqqzzazqzq
  aaqaazaqazqaaqaz
  qqazzazqqaqzzqzq
  zqqzaaqaaazqqaza
  zaqqzqazaqzqaaqq
  zqzzqzqaqzazaqaa
  zzaaqazaaqqzzaqa
  azaazqqqazzzazaa)

;; Enemies
;; TODO Ghost
;; TODO Turtle
;; TODO Cute aliens (Space Invaders 95)
;; TODO Abstract alians (Space Invaders Part II)
;; TODO Slimes
;; TODO Mushroom
;; TODO Wind monster
;; TODO Cute enemies (Kirby)
;; TODO Dinosaur
;; TODO Bee
;; TODO Asteroid
;; TODO Bob-omb
;; TODO Bullet Bill
;; TODO Robot guy
;; TODO Monkey
;; TODO Snake
;; TODO Cute cat things (Gimmick)
(define-sprite sd spr:monster:starguy
  #:w 8 #:h 8
  _ss$$ss_
  _s$ss$s_
  $$ssss$$
  $aaaaaa$
  $$assa$$
  $_$aa$_$
  _$d$$d$_
  $$$__$$$)

(define-sprite sd spr:box-border
  #:w 12 #:h 12
  _$$$$$$$$$$_
  $$aaaaaaaa$$
  $aa$$$$$$aa$
  $a$______$a$
  $a$______$a$
  $a$______$a$
  $a$______$a$
  $a$______$a$
  $a$______$a$
  $aa$$$$$$aa$
  $$aaaaaaaa$$
  _$$$$$$$$$$_)

;; Racing Game
;; TODO Car with different turns (See Retro Game Challenge)

;; TODO Strategy Games (Advance Wars)

;; TODO Pinball

;; xxx allow a drawing mode (already this is very hard)
;; xxx show a grid overlay
;; xxx emacs - save on every keystroke: http://emacs.stackexchange.com/questions/12459/save-buffer-at-each-modification

;; xxx show scenes or combinations of sprites (like tetris blocks)

;; xxx allow shading vs diffuse separation

;; xxx look at http://i.imgur.com/T30sdI1.png

(module+ apse
  (with-apse-params [sd W H]
    (apse-all-sprites 2.0 cw:hi:pals)
    (apse-palettes cw:hi:pals)
    (apse-tile spr:tile:top-down:grass
               #:scale 1.0
               #:pal (list-ref cw:hi:pals 4))
    (apse-animation
     ani:elephant:walking
     #:fps 5.0
     #:palettes (list pal:grayscale))
    (apse-sprite spr:box-border
                 #:palettes (list* pal:mono:white pal:mono:black cw:hi:pals))
    (apse-animation
     ani:elephant:walking
     #:fps 5.0
     #:palettes cw:hi:pals)
    (apse-sprite spr:megaman2
                 #:palettes cw:hi:pals)    
    ))
