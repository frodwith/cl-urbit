# cl-urbit

Nock/urbit runtime in Common Lisp.

# Overview

This is intended as a reference implementation for @frodwith's latest
iteration of the jet dashboard. Along the way, it will produce a couple
of usable artifacts, including:

  * a hoon REPL
  * a replacement urbit-worker process

Currently a toy demo of the hoon REPL is available. Many jets are not
yet implemented, but we can boot an ivory pill, compile simple programs,
and pretty-print the results.

# Hoon REPL Installation

  1. Install [SBCL](http://sbcl.org).
  2. Install [Quicklisp](https://www.quicklisp.org/beta/index.html#installation).
  3. Add this repo to Quicklisp's local projects.

        cd ~/quicklisp/local-projects
        git clone http://github.com/frodwith/cl-urbit.git

  4. Get an [ivory pill](https://github.com/urbit/urbit/tree/master/bin).

        curl -Lo /tmp/ivory.pill https://github.com/urbit/urbit/raw/master/bin/ivory.pill

  5. With [Quicklisp loaded](https://www.quicklisp.org/beta/index.html#loading) in SBCL:

        (ql:quickload :cl-urbit)
        (urbit/hoon/ivory:save-hoon-and-die #P"/tmp/hoon" #P"/tmp/ivory.pill")

# Hoon REPL Usage

Launching with no arguments or the --repl option gives you a REPL with
the ivory pill as the subject. Each line will be compiled and its result
pretty printed. Use rlwrap or similar to get line-editing, etc.

Arguments are treated as hoon source filenames. The files are conceptually
=~'d together, i.e.

    hoon one.hoon two.hoon --repl

makes a subject of

    =~
      <contents-of-one>
      <contents-of-two>
    ==

and plunks you down at a repl. If you don't give the --repl argument but
do give some hoon files, the product of the last one will be pretty-printed.

# License

Copyright (c) 2020 Paul Driver

Licensed under the MIT License.
