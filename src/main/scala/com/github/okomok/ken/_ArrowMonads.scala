

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


private[ken] final class _ArrowMonads[k[-_, +_]](val arrow: ArrowApply[k]) {
    final case class _ArrowMonad[+b](override val get: k[Unit, b]) extends NewtypeOf[k[Unit, b]]

    object _ArrowMonad extends Newtype1[_ArrowMonad, ({type ot[+a] = k[Unit, a]})#ot] with Monad[_ArrowMonad] with ThisIsInstance {
        implicit def dependent[b](k: NewtypeOf[k[Unit, b]]): _ArrowMonad[b] = _ArrowMonad { k.run }

        // Overrides
        //
        // Newtype1
        private[this] type nt[+a] = _ArrowMonad[a]
        private[this] type ot[+a] = k[Unit, a]
        override def newOf[a](ot: Lazy[ot[a]]): nt[a] = _ArrowMonad(ot)
        override def oldOf[a](nt: Lazy[nt[a]]): ot[a] = nt.run
        // Monad
        import arrow.{>>>:, arr}
        private[this] type m[+a] = _ArrowMonad[a]
        override def `return`[a](x: Lazy[a]): m[a] = _ArrowMonad { arr(_ => x) }
        override def op_>>=[a, b](m: m[a])(f: a => m[b]): m[b] = _ArrowMonad {
            m.run >>>: arr((x: a) => (f(x).run, ())) >>>: arrow.app
        }
    }
}
