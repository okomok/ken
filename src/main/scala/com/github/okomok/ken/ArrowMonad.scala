

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


final case class ArrowMonad[k[-_, +_], +a](override val old: k[Unit, a]) extends NewtypeOf[k[Unit, a]]


object ArrowMonad extends ArrowMonadAs with Kind.FunctionLike {
    trait apply[k <: Kind.Function2] extends apply1[k]
    trait apply1[k <: Kind.Function2] extends Kind.Newtype1 {
        override type apply1[+a] = ArrowMonad[k#apply2, a]
        override type oldtype1[+a] = k#apply2[Unit, a]
    }
}


private[ken] sealed trait ArrowMonadAs { this: ArrowMonad.type =>
/*
    implicit def _asNewtype1[k[-_, +_]]: Newtype1[({type L[+a] = ArrowMonad[k, a]})#L, ({type ot[+a] = k[Unit, a]})#ot] = new Newtype1[({type L[+a] = ArrowMonad[k, a]})#L, ({type ot[+a] = k[Unit, a]})#ot] {
        private type nt[+a] = ArrowMonad[k, a]
        private type ot[+a] = k[Unit, a]
        override def newOf[a](ot: Lazy[ot[a]]): nt[a] = ArrowMonad(ot)
        override def oldOf[a](nt: Lazy[nt[a]]): ot[a] = nt.run
    }
*/
    implicit def _asMonad[k[-_, +_]](implicit i: ArrowApply[k]): Monad[({type L[+a] = ArrowMonad[k, a]})#L] = new Monad[({type L[+a] = ArrowMonad[k, a]})#L] {
        import i.{>>>:, arr}
        private type m[+a] = ArrowMonad[k, a]
        override def `return`[a](x: Lazy[a]): m[a] = ArrowMonad { arr(_ => x) }
        override def op_>>=[a, b](m: m[a])(f: a => m[b]): m[b] = ArrowMonad {
            m.run >>>: arr((x: a) => (f(x).run, ())) >>>: i.app
        }
    }
}
