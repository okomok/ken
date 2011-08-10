

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


object WeakIdentity extends MonadFix[({type m[+a] = a})#m] with ThisIsInstance {
    // Overrides
    //
    // Functor
    private[this] type f[+a] = a
    override def fmap[a, b](f: a => b)(m: f[a]): f[b] = f(m)
    // Monad
    private[this] type m[+a] = f[a]
    override def `return`[a](a: => a): m[a] = a
    override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = k(m)
    // MonadFix
    override def mfix[a](f: (=> a) => m[a]): m[a] = Function.fix(f)
}
