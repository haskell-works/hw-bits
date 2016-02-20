# hw-succinct
[![Circle CI](https://circleci.com/gh/haskell-works/hw-succinct.svg?style=svg)](https://circleci.com/gh/haskell-works/hw-succinct)
Conduits for tokenizing streams.

`hw-succinct` is an extension to the `conduit` library based on
`Data.Conduit.Attoparsec` that allows the tokenization of a stream whilst also
tracking arbitrary state.  The library includes in-built support for tracking
such as line-column and stream offset, but can be extended with type-classes to
support alternative state tracking.

For an example, see [`app/Main.hs`](../master/app/Main.hs)

## Prerequisites
* Install `haskell-stack`.
* Install `hlint` (eg. `stack install hlint`)

## Building

Run the following in the shell:

    git clone git@github.com:haskell-works/hw-succinct.git
    cd hw-succinct
    stack setup
    stack build
    stack test
    stack ghci --ghc-options -XOverloadedStrings \
      --main-is hw-succinct:exe:hw-succinct-example

## References
* [Original Pull Request](https://github.com/snoyberg/conduit/pull/244)
* [Typed Tagless Final Interpreters](http://okmij.org/ftp/tagless-final/course/lecture.pdf)
* [Conduit Overview](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/conduit-overview)

## Special mentions
* [Sydney Paper Club](http://www.meetup.com/Sydney-Paper-Club/)
