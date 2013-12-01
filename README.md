Goofy
=====

Goofy is just another functional statically typed programming language.
Key features:
- Hindley-Milner type system (which kinda sucks right now)
- Pattern-matching
- Lexical scopes
- Functions are first-class values
- All functions are curried by default

Goofy's big parents are Haskell and Standard ML.

Building Goofy interpreter
--------------------------

Main build dependency for Goofy is Lazarus. If you haven't already, you should install FPC and Lazarus (`sudo
apt-get install fpc lazarus` works just fine).

Another important dependency is `ndlexyacc` library which we use to generate Goofy lexer and parser. You should
install it by running `./get_lex_yacc.sh` script from repo's root directory.

When all build dependencies are satisfied, you can build Goofy interpreter by running `make` command.
After building, the `goofy` binary is expected to appear in repo's root directory.

Running Goofy REPL
------------------

You can enter Goofy REPL by invoking Goofy interpreter binary with `-r` flag. That is, `./goofy -r`. Alternatively,
you can invoke `make run`, which will rebuild Goofy interpreter and then run the REPL for you.

When you are in REPL, you could enter `:help` command to get a bunch of instructions about how to use the REPL.

Running your Goofy programs
---------------------------

You can run your Goofy programs by invoking Goofy interpreter with path to your program as a parameter.
That is, `./goofy examples/Factorial.gf`.

Language tutorial
-----------------

There is no comprehensive description of Goofy's syntax and semantics at the moment. The only thing you can
do is to read sources from `examples`-folder. If you are familiar with Haskell or Standard ML, examples should be
pretty straightforward for you to understand. You can also read `Prelude.gf`, which is automatically imported
at the very beginning of the interpretation process.
