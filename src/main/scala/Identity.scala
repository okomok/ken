

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


object Identity extends MonadFix[({type m[+a] = a})#m] {
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


/*
final case class StrongIdentity[+a](override val run: a) extends Wrap[a]

object StrongIdentity extends MonadFix[StrongIdentity] {
    def run[a](m: StrongIdentity[a]): a = m.run

// Overrides
    // Functor
    private[this] type f[+a] = StrongIdentity[a]
    override def fmap[a, b](f: a => b)(m: f[a]): f[b] = StrongIdentity { f(run(m)) }
    // Monad
    private[this] type m[+a] = f[a]
    override def `return`[a](a: => a): m[a] = StrongIdentity { a }
    override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = k(run(m))
    // MonadFix
    override def mfix[a](f: (=> a) => m[a]): m[a] = StrongIdentity { Function.fix(run[a] _ compose f) }

// Instances
    implicit val monad: MonadFix[StrongIdentity] = this
}
*/
