

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


private[ken] final class _ArrowMonads[k[-_, +_]](val arrow: ArrowApply[k]) {
    final case class _ArrowMonad[+b](override val get: k[Unit, b]) extends Strong[k[Unit, b]]

    object _ArrowMonad extends Monad[_ArrowMonad] with ThisIsInstance {
        implicit def dependent[b](k: Strong[k[Unit, b]]): _ArrowMonad[b] = _ArrowMonad { k.run }

        // Overrides
        //
        // Monad
        import arrow.{>>>, arr}
        private[this] type m[+a] = _ArrowMonad[a]
        override def `return`[a](x: => a): m[a] = _ArrowMonad { arr(_ => x) }
        override def op_>>=[a, b](m: m[a])(f: a => m[b]): m[b] = _ArrowMonad {
            m.run >>> arr(x => (f(x).run, ())) >>> arrow.app
        }

        // Instances
        //
        implicit val weak: Imply1[_ArrowMonad, ({type d[+a] = k[Unit, a]})#d] = new Imply1[_ArrowMonad, ({type d[+a] = k[Unit, a]})#d] {
            private[this] type p[+a] = _ArrowMonad[a]
            private[this] type d[+a] = k[Unit, a]
            override def imply1[a](p: p[a]): d[a] = p.get
            override def unimply1[a](d: => d[a]): p[a] = _ArrowMonad(d)
        }
    }
}
