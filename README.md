Minishell
=========

[![Made at the Recurse Center](http://img.shields.io/badge/Made_At-The_Recurse_Center-brightgreen.svg)](https://www.recurse.com)

This project is based on the minishell assignment taught by [Phil Nelson][nelson]
at Western Washington University for his Unix Programming class.

[nelson]: http://facultyweb.cs.wwu.edu/~phil/


... in OCaml
------------

The twist is that it's implemented in OCaml.


Dependencies
------------

I recommend installing OCaml via [OPAM]. In particular, I followed [the
instructions][rwoinstall] from the Real World OCaml book. At a
minimum, you must install the core libraries, and probably the
core_extended opam package as well.


    opam install core core_extended


[OPAM]: http://opam.ocaml.org/
[rwoinstall]: https://github.com/realworldocaml/book/wiki/Installation-Instructions


Building
--------

This is super easy. Along with the `core` and `core_extended`
libraries should have come a program called `corebuild`. If it's not
available on your `PATH` then you should make sure that you've done
`eval $(opam config env)` in your current shell.

Then compiling is just:

    corebuild src/minishell.native


Alternatively you could specify `minishell.byte` as the target to
produce the theoretically more debuggable and portable OCaml bytecode
version. Also, this one is slower because the bytecode is interpreted.

Assuming the build was successful, you should be able to just
`./minishell.native` and get the minishell. For maximum awesome,
instead do `rlwrap ./minishell.native`.
