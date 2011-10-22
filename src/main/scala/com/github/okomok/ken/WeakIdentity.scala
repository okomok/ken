

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


object WeakIdentity extends MonadFix[({type m[+a] = a})#m] with Extend[({type m[+a] = a})#m] {
    // Overrides
    //
    // Functor
    private type f[+a] = a
    override def fmap[a, b](f: a => b): f[a] => f[b] = m => f(m)
    // Monad
    private type m[+a] = a
    override def `return`[a](a: Lazy[a]): m[a] = a
    override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = k(m)
    // MonadFix
    override def mfix[a](f: Lazy[a] => m[a]): m[a] = Function.fix(f)
    // Extend
    private type w[+a] = a
    override def duplicate[a](w: w[a]): w[w[a]] = w
}
