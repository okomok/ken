

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Identity[+a] {
    def run: a
}


trait IdentityProxy[+a] extends Identity[a] with Proxy {
    override def self: Identity[a]
    override def run: a = self.run
}


object Identity extends MonadFix[Identity] {
    def apply[a](a: a): Identity[a] = new Identity[a] {
        override def run: a = a
    }

    def run[a](m: Identity[a]): a = m.run

// Overrides
    // Functor
    private[this] type f[+a] = Identity[a]
    override def fmap[a, b](f: a => b)(m: f[a]): f[b] = Identity { f(run(m)) }
    // Monad
    private[this] type m[+a] = f[a]
    override def `return`[a](a: => a): m[a] = Identity { a }
    override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = k(run(m))
    // MonadFix
    override def mfix[a](f: (=> a) => m[a]): m[a] = Identity { Function.fix(run[a]_ compose f) }

// Instances
    implicit val monad: MonadFix[Identity] = this

    implicit val weak: Weak[Identity, ({type d[+a] = a})#d] = new Weak[Identity, ({type d[+a] = a})#d] {
        type p[+a] = Identity[a]
        type d[+a] = a
        override def wrap[a](d: => d[a]): p[a] = Identity(d)
        override def unwrap[a](p: p[a]): d[a] = run(p)

        override implicit def functor(implicit i: Functor[p]): Functor[d] = WeakIdentity
        override implicit def applicative(implicit i: Applicative[p]): Applicative[d] = WeakIdentity
        override implicit def monad(implicit i: Monad[p]): Monad[d] = WeakIdentity
        override implicit def monadFix(implicit i: MonadFix[p]): MonadFix[d] = WeakIdentity
    }
}


object WeakIdentity extends MonadFix[({type m[+a] = a})#m] {
// Overrides
    // Functor
    private[this] type f[+a] = a
    override def fmap[a, b](f: a => b)(m: f[a]): f[b] = f(m)
    // Monad
    private[this] type m[+a] = f[a]
    override def `return`[a](a: => a): m[a] = a
    override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = k(m)
    // MonadFix
    override def mfix[a](f: (=> a) => m[a]): m[a] = Function.fix(f)

// Instances
    implicit val monad: MonadFix[({type m[+a] = a})#m] = this
}
