

// Copyright Shunsuke Sogame 2011.
//
// (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package parsec


object Parsers extends ParsersBase

trait ParsersBase extends ParsecsBase[String, Unit] {
    type Parser[+a] = Parsec[a]
    final val Parser = Parsec

    def parseFromFile[a](p: Parser[a]): IO.FilePath => IO[Either[ParseError, a]] = fname => {
        import IO.`for`
        for { input <- IO.readFile(fname) } yield runParser(p)(())(fname)(input)
    }
}
