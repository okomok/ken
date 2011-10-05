

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken
package parsec


// type Parsec[s, u, +a] = ParsecT[s, u, WeakIdentity.apply, a]


@Annotation.compilerWorkaround("2.9.1", 5031)
object _Parsec extends Kind.FunctionLike {
    trait apply2[s, u] extends ParsecT.apply3[s, u, WeakIdentity.apply]

    def apply[s, u, a](n: UnParser[s, u, WeakIdentity.apply, a]): Parsec[s, u, a] = ParsecT(n)
    def unapply[s, u, a](m: Parsec[s, u, a]): Option[UnParser[s, u, WeakIdentity.apply, a]] = Some(m.run)
}
