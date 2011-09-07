

// Copyright Shunsuke Sogame 2011.
//
// Copyright 2004, The University Court of the University of Glasgow.
// All rights reserved.
//
// Copyright (c) 2002 Simon Peyton Jones
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait Typeable[a] extends Typeclass0[a] { outer =>
    final val asTypeable: Typeable[a] = this

    // Core
    //
    type typeOf = Lazy[a] => TypeRep
    def typeOf: typeOf
}


trait TypeableProxy[a] extends Typeable[a] {
    def selfTypeable: Typeable[a]

    override def typeOf: typeOf = selfTypeable.typeOf
}


object Typeable extends TypeableInstance {
    // For some reason, result type-ascription doesn't work.
    def cast[a, b](x: => a, y: Type[b])(implicit i: Typeable[a], j: Typeable[b]): Maybe[b] = {
        lazy val r: Maybe[b] = if (i.typeOf(x) <:< j.typeOf(Lazy(Maybe.fromJust(r)))) {
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


sealed trait TypeableInstance { this: Typeable.type =>
    implicit def of[a](implicit i: ClassManifest[a]): Typeable[a] = new Typeable[a] {
        override val typeOf: typeOf = _ => i
    }
}
