

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


final case class Identity[+a](override val get: a) extends Strong[a]


object Identity extends Newtype1[Identity, ({type ot[+a] = a})#ot] with MonadFix[Identity] with ThisIsInstance {
    // Overrides
    //
    // Newtype1
    private type nt[+a] = Identity[a]
    private type ot[+a] = a
    override def newOf[a](ot: Lazy[ot[a]]): nt[a] = Identity(ot)
    override def oldOf[a](nt: Lazy[nt[a]]): ot[a] = nt.run
    // Functor
    private type f[+a] = Identity[a]
    override def fmap[a, b](f: a => b)(m: f[a]): f[b] = Identity { f(run(m)) }
    // Monad
    private type m[+a] = Identity[a]
    override def `return`[a](a: Lazy[a]): m[a] = Identity { a }
    override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = k(run(m))
    // MonadFix
    override def mfix[a](f: Lazy[a] => m[a]): m[a] = Identity { Function.fix(run[a]_ `.` f) }
}
