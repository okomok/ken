

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


private[ken] final class _ArrowMonads[a[-_, +_]](val arrow: ArrowApply[a]) {
    final case class _ArrowMonad[+b](override val get: a[Unit, b]) extends Strong[a[Unit, b]]

    object _ArrowMonad extends Monad[_ArrowMonad] with ThisIsInstance {
        import arrow.{>>>, arr}
        private[this] type m[+a] = _ArrowMonad[a]
        override def `return`[a](x: => a): m[a] = _ArrowMonad { arr(_ => x) }
        override def op_>>=[a, b](m: m[a])(f: a => m[b]): m[b] = _ArrowMonad {
            m.run >>> arr(x => { val h = f(x); (h.run, ()) }) >>> arrow.app
        }
    }
}
