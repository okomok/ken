

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


final case class Identity[+a](override val old: a) extends NewtypeOf[a]


object Identity extends Newtype1[Identity, ({type ot[+a] = a})#ot] with MonadFix[Identity] with Extend[Identity] with ThisIsInstance {
    // Overrides
    //
    // Newtype1
    private type nt[+a] = Identity[a]
    private type ot[+a] = a
    override def newOf[a](ot: Lazy[ot[a]]): nt[a] = Identity(ot)
    override def oldOf[a](nt: Lazy[nt[a]]): ot[a] = nt.run
    // Functor
    private type f[+a] = Identity[a]
    override def fmap[a, b](f: a => b): f[a] => f[b] = m => Identity { f(run(m)) }
    // Monad
    private type m[+a] = Identity[a]
    override def `return`[a](a: Lazy[a]): m[a] = Identity { a }
    override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = k(run(m))
    // MonadFix
    override def mfix[a](f: Lazy[a] => m[a]): m[a] = Identity { Function.fix(run[a]_ `.` f) }
    // Extend
    private type w[+a] = Identity[a]
    override def duplicate[a](w: w[a]): w[w[a]] = Identity(w)

    // Instances
    //
    implicit def _asNewtype0[a]: Newtype0[Identity[a], a, Kind.Nil] = new Newtype0[Identity[a], a, Kind.Nil] {
        override val newOf: newOf = ot => Identity(ot)
        override val oldOf: oldOf = nt => nt.get
    }
}
