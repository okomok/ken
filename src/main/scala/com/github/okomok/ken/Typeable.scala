

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


trait Typeable[a] extends Typeclass0[a] {
    final val asTypeable: Typeable[a] = this

    // Core
    //
    type typeOf = Lazy[a] => TypeRep
    def typeOf: typeOf

    // Extra
    //
    def cast[b](x: Lazy[a])(implicit j: Typeable[b]): Maybe[b] = {
        lazy val r: Maybe[b] = if (typeOf(x) <:< j.typeOf(Lazy(Maybe.fromJust(r)))) {
            Just(x.!.asInstanceOf[b])
        } else {
            Nothing
        }
        r
    }

    // In short, success only if a == b.
    def mkT[b](f: b => b)(x: a)(implicit j: Typeable[b]): a = cast[b](x)(j) match {
        case Nothing => x
        case Just(y) => j.cast[a](f(y))(this) match {
            case Nothing => x
            case Just(r) => r
        }
    }

    def mkQ[b, r](r: r)(q: b => r)(a: a)(implicit j: Typeable[b]): r = cast[b](a)(j) match {
        case Nothing => r
        case Just(b) => q(b)
    }
}


trait TypeableProxy[a] extends Typeable[a] {
    def selfTypeable: Typeable[a]

    override def typeOf: typeOf = selfTypeable.typeOf

    override def cast[b](x: Lazy[a])(implicit j: Typeable[b]): Maybe[b] = selfTypeable.cast(x)(j)
    override def mkT[b](f: b => b)(x: a)(implicit j: Typeable[b]): a = selfTypeable.mkT(f)(x)(j)
    override def mkQ[b, r](r: r)(q: b => r)(a: a)(implicit j: Typeable[b]): r = selfTypeable.mkQ(r)(q)(a)(j)
}


object Typeable extends TypeableInstance with TypeableShortcut {
    def apply[a <: Kind.Function0](implicit i: Typeable[a#apply0]): Typeable[a#apply0] = i
}


sealed trait TypeableInstance { this: Typeable.type =>
    implicit def of[a](implicit i: ClassManifest[a]): Typeable[a] = new Typeable[a] {
        override val typeOf: typeOf = _ => i
    }
}


sealed trait TypeableShortcut { this: Typeable.type =>
    // For some reason, result type-ascription doesn't work.
    def cast[a, b](x: Lazy[a], y: Type[b] = null)(implicit i: Typeable[a], j: Typeable[b]): Maybe[b] = i.cast[b](x)(j)

    def mkT[a, b](f: b => b)(x: a)(implicit i: Typeable[a], j: Typeable[b]): a = i.mkT(f)(x)(j)
    def mkQ[a, b, r](r: r)(q: b => r)(a: a)(implicit i: Typeable[a], j: Typeable[b]): r = i.mkQ(r)(q)(a)(j)

    // Rejected because Typeable[a => a] is bothersome.
    private def _original_mkT[a, b](f: b => b)(x: a)(implicit i: Typeable[a => a], j: Typeable[b => b]): a = cast(f, Type[a => a]) match {
        case Nothing => x
        case Just(g) => g(x)
    }
}
