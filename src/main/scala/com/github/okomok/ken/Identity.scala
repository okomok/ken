

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


final case class Identity[+a](override val get: a) extends Strong[a]


object Identity extends Kind.Strong1 with MonadFix[Identity] with ThisIsInstance {
    override type weak[+a] = a

    def run[a](m: Identity[a]): a = m.run

    // Overrides
    //
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
    //
    implicit val weak: Imply1[Identity, ({type d[+a] = a})#d] = new Imply1[Identity, ({type d[+a] = a})#d] {
        type p[+a] = Identity[a]
        type d[+a] = a
        override def imply1[a](p: p[a]): d[a] = run(p)
        override def unimply1[a](d: => d[a]): p[a] = Identity(d)

        override def asFunctor(implicit i: Functor[p]): Functor[d] = WeakIdentity
        override def asApplicative(implicit i: Applicative[p]): Applicative[d] = WeakIdentity
        override def asMonad(implicit i: Monad[p]): Monad[d] = WeakIdentity
        override def asMonadFix(implicit i: MonadFix[p]): MonadFix[d] = WeakIdentity
    }
}
