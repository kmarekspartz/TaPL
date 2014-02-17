# TaPL

Implementations of languages referred to in [Types and Programming Languages](http://www.cis.upenn.edu/~bcpierce/tapl/) by [Benjamin C. Pierce](http://www.cis.upenn.edu/~bcpierce/).


## Goals

1. Learn about Haskell

    Prior to this project, I had not written any multi-file Haskell projects, so this was a way for me to learn about how to set up and organize a Haskell project.

2. Learn about types and programming languages

    Working through the book is a good way to learn the content of the book.

3. Provide reference implementations of the various type systems

    I plan on implementing languages which use these type systems in the future, so I would like to be able to reuse this code.


## Contributions

Please do! I would love contributions particularly on the following:

- Making the code more idiomatic
- Properties to test
- Project structure
- Implementation details


## Languages

Language     | Figure # 
------------ | --------
Boolean      | 3-1
Arith        | 3-2
TypedBoolean | 8-1
TypedArith   | 8-2

<!-- TODO: Put in complete list of languages from the book, with page #, Figure #, and an Implemented column. -->


## Tests

To run the tests, use `./test`, which runs `cabal install`, tests each language, `hlint`s, and `grep`s for tabs.


## Previous project

This project was initially based on a previous [tapl-haskell](https://code.google.com/p/tapl-haskell/) project by [Ryan W. Porter](http://www.ryanwporter.com/). That project, like this one, avoids the use of Template-Haskell, but instead it uses a different form of code-generation. For this project, I'd like to avoid code-generation (at least initially), so that each individual language can be considered on its own.

This allows for a `diff` of the implementations to be compared to the figures in the book, since the figures typically show only differences between the new language and the parent language. Use `diff` on the implementations as follows:

    diff src/Language/TaPL/Boolean src/Language/TaPL/Arith
