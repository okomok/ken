

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


trait Typeable[a] extends Typeclass[a] {
    final val asTypeable: Typeable[a] = this

    // Core
    //
    type typeOf = Lazy[_] => TypeRep
    def typeOf: typeOf
}


trait TypeableProxy[a] extends Typeable[a] {
    def selfTypeable: Typeable[a]

    override def typeOf: typeOf = selfTypeable.typeOf
}


object Typeable extends TypeableInstance with TypeableShortcut {
    def apply[a <: Kind.Function0](implicit i: Typeable[a#apply0]): Typeable[a#apply0] = i

    // For some reason, result type-ascription doesn't work.
    def cast[a, b](x: Lazy[a], y: Type[b] = null)(implicit i: Typeable[a], j: Typeable[b]): Maybe[b] = {
        lazy val r: Maybe[b] = {
            if (i.typeOf(x) <:< j.typeOf(Lazy(Maybe.fromJust(r)))) Just(x.!.asInstanceOf[b])
            else Nothing
        }
        r
    }

    // Any usecase?
    def gcast[a, b, c[_]](x: Lazy[c[a]], y: Type[c[b]] = null)(implicit i: Typeable[a], j: Typeable[b]): Maybe[c[b]] = {
        def getArg[x](x: c[x]): x = undefined
        lazy val r: Maybe[c[b]] = {
            if (i.typeOf(getArg(x)) <:< j.typeOf(getArg(Maybe.fromJust(r)))) Just(x.!.asInstanceOf[c[b]])
            else Nothing
        }
        r
    }

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

    // Rejected because Typeable[a => a] is bothersome.
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


private[ken] sealed trait TypeableShortcut { this: Typeable.type =>
    def typeOf[a](x: Lazy[a])(implicit i: Typeable[a]): TypeRep = i.typeOf(x)
}
