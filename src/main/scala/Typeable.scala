

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Typeable[a] {
    def typeOf(x: => a): ClassManifest[a]
}


trait TypeableProxy[a] extends Typeable[a] with Proxy {
    def self: Typeable[a]
    override def typeOf(x: => a): ClassManifest[a] = self.typeOf(x)
}


object Typeable {
    class Instance[a](implicit ac: ClassManifest[a]) extends Typeable[a] {
        override def typeOf(x: => a): ClassManifest[a] = ac
    }
    implicit def instance[a](implicit ac: ClassManifest[a]): Typeable[a] = new Instance[a]

    // For some reason, result type-ascription doesn't work.
    def cast[a, b](x: => a, y: Type[b])(implicit ac: Typeable[a], bc: Typeable[b]): Maybe[b] = {
        lazy val r: Maybe[b] = if (ac.typeOf(x) <:< bc.typeOf(Maybe.fromJust(r))) {
            Just(x.asInstanceOf[b])
        } else {
            Nothing
        }
        r
    }

    // Typeable[a => a] is bothersome.
    def _mkT[a, b](f: b => b)(x: a)(implicit ac: Typeable[a => a], bc: Typeable[b => b]): a = cast(f, Type[a => a]) match {
        case Nothing => x
        case Just(g) => g(x)
    }

    // In short, success only if a == b.
    def mkT[a, b](f: b => b)(x: a)(implicit ac: Typeable[a], bc: Typeable[b]): a = cast(x, Type[b]) match {
        case Nothing => x
        case Just(y) => cast(f(y), Type[a]) match {
            case Nothing => x
            case Just(r) => r
        }
    }

    def mkQ[a, b, r](r: r)(q: b => r)(a: a)(implicit ac: Typeable[a], bc: Typeable[b]): r = cast(a, Type[b]) match {
        case Nothing => r
        case Just(b) => q(b)
    }
}
