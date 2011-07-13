

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


/**
 * Identity Monad (not wrapped)
 */
object Identity extends MonadFix[({type f[+a] = a})#f] {
    override implicit def instance = this

    // Functor
    private[this] type f[+a] = a
    override def fmap[a, b](f: a => b)(m: f[a]): f[b] = f(m)
    // Monad
    private[this] type m[+a] = f[a]
    override def `return`[a](a: a): m[a] = a
    override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = k(m)
    // MonadFix
    override def mfix[a](f: (=> a) => m[a]): m[a] = Function.fix(f)
}


/*
sealed abstract class Identity[+a] {
    def run: a
}

object Identity extends MonadFix[Identity] {
    override implicit def instance = this

    def apply[a](r: a): Identity[a] = new Identity[a] {
        override def run: a = r
    }

    def runIdentity[a](m: Identity[a]): a = m.run

    // Functor
    private[this] type f[+a] = Identity[a]
    override def fmap[a, b](f: a => b)(m: f[a]): f[b] = Identity(f(runIdentity(m)))
    // Monad
    private[this] type m[+a] = f[a]
    override def `return`[a](a: a): m[a] = Identity(a)
    override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = k(runIdentity(m))
    // MonadFix
    override def mfix[a](f: (=> a) => m[a]): m[a] = Identity(Function.fix(runIdentity[a] _ compose f))
}
*/
