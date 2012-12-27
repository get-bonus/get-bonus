Get Bonus is a project about discovering the best way to program video
games.

For more information, refer to the
[Wiki](https://github.com/get-bonus/get-bonus/wiki)

Currently, we only support Linux, because of the way I deal with
controllers and sound. If you'd like to patch in support on other
platforms, email me.

You can install it via a Racket package:
````raco pkg install get-bonus````

When you run it, you'll run
````racket -l gb/main````

However, you'll need to get a few things before you can run it:

1. The OS dependencies of `racket/draw` listed on
[this page](http://docs.racket-lang.org/draw/libs.html).

1. The OS dependencies of `racket/gui` listed on
[this page](http://docs.racket-lang.org/gui/libs.html).

1. OpenAL runtime libraries.

1. Copyrighted images files used to construct `r.png` from
`r.src`. If you don't have these, then `r.free.png` will
be used automatically.
