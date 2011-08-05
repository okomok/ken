

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


object Function {
    type apply[z] = Kind.Function1 {
        type apply[+a] = Function1[z, a]
    }

    def fix[a](f: (=> a) => a): a = {
        lazy val x: a = f(x)
        x
    }

    def fixfun[a](f: (a => a) => a => a): a => a = { x => f(fixfun(f))(x) }

    def on[a, b, c](* : b => b => c)(f: a => b): a => a => c = { x => y => *(f(x))(f(y)) }

    implicit def monad[z]: MonadReader[z, ({type m[+a] = z => a})#m] = new MonadReader[z, ({type m[+a] = z => a})#m] {
        // Functor
        private[this] type f[+a] = z => a
        override def fmap[a, b](x: a => b)(y: f[a]): f[b] = x compose y
        // Applicative
        override def pure[a](x: => a): f[a] = const(x)
        override def op_<*>[a, b](x: f[a => b])(y: f[a]): f[b] = { z => x(z)(y(z)) }
        // Monad
        private[this] type m[+a] = f[a]
        override def `return`[a](x: => a): m[a] = const(x)
        override def op_>>=[a, b](f: m[a])(k: a => m[b]): m[b] = { z => k(f(z))(z) }
        // MonadReader
        override def ask: m[z] = ken.id
        override def local[a](f: z => z)(m: m[a]): m[a] = m compose f
    }

    implicit def asMonoid[z, b](implicit mb: Monoid[b]): Monoid[z => b] = new Monoid[z => b] {
        private[this] type m = z => b
        override val mempty: m = _ => mb.mempty
        override val mappend: m => (=> m) => m = { x => y =>
            val y_ = y // scalac mistery
            z => mb.mappend(x(z))(y_(z))
        }
    }
}
