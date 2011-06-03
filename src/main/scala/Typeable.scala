

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


sealed abstract class Typeable[a] {
    def typeOf(x: => a): ClassManifest[a]
}

object Typeable {
    implicit def instanceOfAny[a](implicit ac: ClassManifest[a]): Typeable[a] = new Typeable[a] {
        override def typeOf(x: => a): ClassManifest[a] = ac
    }

    def cast[a, b](x: a)(implicit ac: Typeable[a], bc: Typeable[b]): Maybe[b] = {
        lazy val r: Maybe[b] = if (ac.typeOf(x) <:< bc.typeOf(Maybe.fromJust(r))) {
            Maybe.Just(x.asInstanceOf[b])
        } else {
            Maybe.Nothing
        }
        r
    }

    def mkT[a, b](f: b => b)(x: a)(implicit ac: Typeable[a => a], bc: Typeable[b => b]): a = cast[b => b, a => a](f) match {
        case Maybe.Nothing => x
        case Maybe.Just(g) => g(x)
    }
}
