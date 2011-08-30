

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


// Parsec: http://legacy.cs.uu.nl/daan/parsec.html


import parsec._


final class Parsec[n <: Kind.Function1](override implicit val inner: Monad[n#apply]) extends ParsecOf[n]

trait ParsecOf[n <: Kind.Function1] extends _Parsec[n#apply]


private[ken] trait _Parsec[n[+_]]
    extends _Error[n] with _Pos[n]
{
    val inner: Monad[n]
}
