## WIP Scala 3 support

For now, this relies on the
[Scala version decoupling work](https://github.com/lihaoyi/Ammonite/pull/1135),
so that only a few specific modules need to be cross-compiled for Scala 3. The
REPL itself still uses Scala 2. It class-loads upon startup the Scala 3
user-facing API and the Scala 3 compiler module, and interact with those via
Java interfaces.

What works in manual tests:
- syntax highlighting
- import $ivy-s
- simple scripts (no entrypoints)
- …

What doesn't work yet:
- completions
- line numbers in compiler errors
- many tests don't pass yet
- some new Scala 3 features probably don't work yet (import givens? more? this needs to be checked)
- …


How to run the Scala 3 REPL:
```text
$ ./mill -i 'amm.cross[3.0.0-M2].run'
```

How to run cross-tests:
```text
$ ./mill 'amm.repl.cross-tests[3.0.0-M2].test'
```
