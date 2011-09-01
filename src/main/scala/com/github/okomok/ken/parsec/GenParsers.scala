

// Copyright Shunsuke Sogame 2011.
//
// (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package parsec


final class GenParsers[tok, st] extends GenParsersBase[tok, st]

trait GenParsersBase[tok, st] extends ParsecsBase[List[tok], st] {
    type GenParser[+a] = Parsec[a]
    final val GenParser = Parsec
}
