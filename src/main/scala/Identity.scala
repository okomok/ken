

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


final case class Identity[+a](override val run: a) extends Wrap[a]

object Identity extends MonadFix[Identity] {
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
    override def mfix[a](f: (=> a) => m[a]): m[a] = Identity { Function.fix(run[a] _ compose f) }

// Instances
    implicit val monad: MonadFix[Identity] = this
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
