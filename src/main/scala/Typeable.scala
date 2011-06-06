

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Typeable[a] {
    def typeOf(x: => a): ClassManifest[a]
}

object Typeable {
    implicit def instanceOfAny[a](implicit ac: ClassManifest[a]): Typeable[a] = new Typeable[a] {
        override def typeOf(x: => a): ClassManifest[a] = ac
    }

    def cast[a, b](x: => a)(implicit ac: Typeable[a], bc: Typeable[b]): Maybe[b] = {
        lazy val r: Maybe[b] = if (ac.typeOf(x) <:< bc.typeOf(Maybe.fromJust(r))) {
            Just(x.asInstanceOf[b])
        } else {
            Nothing
        }
        r
    }

    // Typeable[a => a] is bothersome.
    def _mkT[a, b](f: b => b)(x: a)(implicit ac: Typeable[a => a], bc: Typeable[b => b]): a = cast[b => b, a => a](f) match {
        case Nothing => x
        case Just(g) => g(x)
    }

    // In short, success only if a == b.
    def mkT[a, b](f: b => b)(x: a)(implicit ac: Typeable[a], bc: Typeable[b]): a = cast[a, b](x) match {
        case Nothing => x
        case Just(y) => cast[b, a](f(y)) match {
            case Nothing => x
            case Just(r) => r
        }
    }

    def mkQ[a, b, r](r: r)(q: b => r)(a: a)(implicit ac: Typeable[a], bc: Typeable[b]): r = cast[a, b](a) match {
        case Nothing => r
        case Just(b) => q(b)
    }
}
