Demonstrate how to write a basic parser for the untyped lambda
calculus in Haskell, using the two seemingly most-popular options
available: Parsec and Happy/Alex.

To build:

```
cabal configure
cabal build
```

To run

```
./dist/build/lambda/lambda < examples/ex1.lam
```

This will run each of the parsers and print the AST and evaluation.

These parsers were created because I could not find an example of how
to parse expressions with an implicit "juxtaposition" operator --
i.e., an expression formed by two expressions side-by-side, e.g.,
function application in ML-like languages. It turns out to be pretty easy.

Note that these parsers _should_ accept exactly the same grammar and
generate identical ASTs... but that is a difficult thing to check, so
no guarantees! In fact, if you can spot a way in which they are
different, I would be very curious to know about it.

I do not claim or expect that these parsers are efficient.
