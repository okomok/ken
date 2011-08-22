

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


// Draft


trait Typeable[a] extends Typeclass0[a] {
    final val asTypeable: Typeable[a] = this

    // Core
    //
    def typeOf(x: => a): ClassManifest[a]
}


trait TypeableProxy[a] extends Typeable[a] {
    def selfTypeable: Typeable[a]

    override def typeOf(x: => a): ClassManifest[a] = selfTypeable.typeOf(x)
}


object Typeable extends TypeableInstance {
    def apply[a <: Kind.Function0](implicit i: Typeable[a#apply0]): Typeable[a#apply0] = i

    // For some reason, result type-ascription doesn't work.
    def cast[a, b](x: => a, y: Type[b])(implicit i: Typeable[a], j: Typeable[b]): Maybe[b] = {
        lazy val r: Maybe[b] = if (i.typeOf(x) <:< j.typeOf(Maybe.fromJust(r))) {
            Just(x.asInstanceOf[b])
        } else {
            Nothing
        }
        r
    }

    // In short, success only if a == b.
    def mkT[a, b](f: b => b)(x: a)(implicit i: Typeable[a], j: Typeable[b]): a = cast(x, Type[b]) match {
        case Nothing => x
        case Just(y) => cast(f(y), Type[a]) match {
            case Nothing => x
            case Just(r) => r
        }
    }

    def mkQ[a, b, r](r: r)(q: b => r)(a: a)(implicit i: Typeable[a], j: Typeable[b]): r = cast(a, Type[b]) match {
        case Nothing => r
        case Just(b) => q(b)
    }

    // Typeable[a => a] is bothersome.
    private def _original_mkT[a, b](f: b => b)(x: a)(implicit i: Typeable[a => a], j: Typeable[b => b]): a = cast(f, Type[a => a]) match {
        case Nothing => x
        case Just(g) => g(x)
    }

}


private[ken] trait TypeableInstance { this: Typeable.type =>
    implicit def _ofAny[a](implicit i: ClassManifest[a]): Typeable[a] = new Typeable[a] {
        override def typeOf(x: => a): ClassManifest[a] = i
    }
}
