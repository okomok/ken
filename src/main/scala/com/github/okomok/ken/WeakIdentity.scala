

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


object WeakIdentity extends MonadFix[({type m[+a] = a})#m] {
    // Overrides
    //
    // Functor
    private type f[+a] = a
    override def fmap[a, b](f: a => b)(m: f[a]): f[b] = f(m)
    // Monad
    private type m[+a] = a
    override def `return`[a](a: Lazy[a]): m[a] = a
    override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = k(m)
    // MonadFix
    override def mfix[a](f: Lazy[a] => m[a]): m[a] = Function.fix(f)
}
