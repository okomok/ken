

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


// type Free[f[+_], n[+_], +a] = FreeT[f, WeakIdentity.apply, a]


@Annotation.compilerWorkaround("2.9.1", 5031)
object _Free extends FreeTOp with Kind.FunctionLike {
    trait apply[f[+_]] extends apply1[f]
    trait apply1[f[+_]] extends FreeT.apply2[f, WeakIdentity.apply]

    def apply[f[+_], a](n: Either[a, f[Free[f, a]]]): Free[f, a] = new FreeT[f, WeakIdentity.apply, a](n)
    def unapply[f[+_], a](m: Free[f, a]): Option[Either[a, f[Free[f, a]]]] = Some(m.run)
}
