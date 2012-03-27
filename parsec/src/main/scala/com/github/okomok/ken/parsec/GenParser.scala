

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken
package parsec


// type GenParser[tok, st, +a] = Parsec[List[tok], st, a]


// @scalacWorkaround("2.9.1", 5031)
object _GenParser extends Kind.FunctionLike {
    type apply2[tok, st] = Parsec.apply2[List[tok], st]

    def apply[tok, st, a](n: UnParser[List[tok], st, WeakIdentity.apply, a]): GenParser[tok, st, a] = Parsec(n)
    def unapply[tok, st, a](m: GenParser[tok, st, a]): Option[UnParser[List[tok], st, WeakIdentity.apply, a]] = Some(m.run)
}
