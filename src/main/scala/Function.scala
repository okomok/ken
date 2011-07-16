

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


object Function {
    def fix[a](f: (=> a) => a): a = {
        lazy val x: a = f(x)
        x
    }

    def fixfun[a](f: (a => a) => a => a): a => a = { x => f(fixfun(f))(x) }

    def on[a, b, c](* : b => b => c)(f: a => b): a => a => c = { x => y => *(f(x))(f(y)) }

    implicit def monad[r]: MonadReader[r, ({type m[+a] = r => a})#m] = new MonadReader[r, ({type m[+a] = r => a})#m] {
        // Functor
        private[this] type f[+a] = r => a
        override def fmap[a, b](x: a => b)(y: f[a]): f[b] = x compose y
        // Applicative
        override def pure[a](x: => a): f[a] = const(x)
        override def op_<*>[a, b](x: f[a => b])(y: f[a]): f[b] = { r => x(r)(y(r)) }
        // Monad
        private[this] type m[+a] = f[a]
        override def `return`[a](x: a): m[a] = const(x)
        override def op_>>=[a, b](f: m[a])(k: a => m[b]): m[b] = { r => k(f(r))(r) }
        // MonadReader
        override def ask: m[r] = ken.id
        override def local[a](f: r => r)(m: m[a]): m[a] = m compose f
    }

    implicit def toMonad[r, a](f: r => a): MonadMethod[({type m[+a] = r => a})#m, a] = new MonadMethod[({type m[+a] = r => a})#m, a] {
        override val klass = monad[r]
        override def callee = f
    }

    implicit def monoid[z, b](implicit mb: Monoid[b]): Monoid[z => b] = new Monoid[z => b] {
        private[this] type m = z => b
        override def mempty: m = _ => mb.mempty
        override def mappend(x: m)(y: => m): m = z => mb.mappend(x(z))(y(z))
    }
}
