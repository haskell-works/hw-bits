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

## Memory benchmark

### Parsing large Json files in Scala with Argonaut

         S0C      S1C      S0U        EC       EU        OU      MC      MU    CCSC   CCSU  YGC   YGCT FGC  FGCT   GCT CMD
    -------- -------- -------- --------- -------- --------- ------- ------- ------- ------- --- ------ --- ----- ----- ---------------------------------------------------------------
    174592.0 174592.0      0.0 1048576.0  80526.3   76163.6 73176.0 72338.6 13312.0 13058.6   5  0.065   5 0.278 0.343
    174592.0 174592.0      0.0 1048576.0 536660.4   76163.6 73176.0 72338.6 13312.0 13058.6   5  0.065   5 0.278 0.343 import java.io._, argonaut._, Argonaut._
    174592.0 174592.0      0.0 1048576.0 552389.1   76163.6 73176.0 72338.6 13312.0 13058.6   5  0.065   5 0.278 0.343 val file = new File("/Users/jky/Downloads/78mbs.json"
    174592.0 174592.0      0.0 1048576.0 634066.5   76163.6 73176.0 72338.6 13312.0 13058.6   5  0.065   5 0.278 0.343 val array = new Array[Byte](file.length.asInstanceOf[Int])
    174592.0 174592.0      0.0 1048576.0 644552.3   76163.6 73176.0 72338.6 13312.0 13058.6   5  0.065   5 0.278 0.343 val is = new FileInputStream("/Users/jky/Downloads/78mbs.json")
    174592.0 174592.0      0.0 1048576.0 655038.1   76163.6 73176.0 72338.6 13312.0 13058.6   5  0.065   5 0.278 0.343 is.read(array)
    409600.0 457216.0 294976.0  483328.0 160159.7 1100365.0 80344.0 79310.8 14080.0 13748.1  10  0.633   5 0.278 0.911 val json = new String(array)
    429568.0 438272.0 285182.9  521216.0 146392.6 1956264.5 84184.0 82679.8 14592.0 14099.6  22  1.736   6 1.393 3.129 val data = Parse.parse(json)

## References
* [Original Pull Request](https://github.com/snoyberg/conduit/pull/244)
* [Typed Tagless Final Interpreters](http://okmij.org/ftp/tagless-final/course/lecture.pdf)
* [Conduit Overview](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/conduit-overview)


## Special mentions
* [Sydney Paper Club](http://www.meetup.com/Sydney-Paper-Club/)
