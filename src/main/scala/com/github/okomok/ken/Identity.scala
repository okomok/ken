

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


final case class Identity[+a](override val old: a) extends NewtypeOf[a]


object Identity extends Newtype1[Identity, ({type ot[+a] = a})#ot] with MonadFix[Identity] with Comonad[Identity] with ThisIsInstance {
    // Overrides
    //
    // Newtype1
    private type nt[+a] = Identity[a]
    private type ot[+a] = a
    override def newOf[a](ot: Lazy[ot[a]]): nt[a] = Identity(ot)
    override def oldOf[a](nt: Lazy[nt[a]]): ot[a] = nt.old
    // Functor
    private type f[+a] = Identity[a]
    override def fmap[a, b](f: a => b): f[a] => f[b] = m => Identity(f(m.old))
    // Applicative
    override def pure[a](a: Lazy[a]): f[a] = Identity(a)
    override def op_<*>[a, b](f: f[a => b]): f[a] => f[b] = m => Identity((f.old)(m.old))
    // Monad
    private type m[+a] = Identity[a]
    override def `return`[a](a: Lazy[a]): m[a] = Identity(a)
    override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = k(m.old)
    // MonadFix
    override def mfix[a](f: Lazy[a] => m[a]): m[a] = Identity { Function.fix(run[a]_ `.` f) }
    // Extend
    private type w[+a] = Identity[a]
    override def duplicate[a](w: w[a]): w[w[a]] = Identity(w)
    // Comonad
    override def extract[a](w: w[a]): a = w.old

    // Instances
    //
    implicit def _asNewtype[a]: Newtype[Identity[a], a, Kind.Nil] = new Newtype[Identity[a], a, Kind.Nil] {
        override val newOf: newOf = ot => Identity(ot)
        override val oldOf: oldOf = nt => nt.old
    }
}
