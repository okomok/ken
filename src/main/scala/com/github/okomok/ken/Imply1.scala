

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


/**
 * Implements `d` instances from `p` instances.
 */
trait Imply1[p[+_], d[+_]] extends Typeclass with Imply1Instance[p, d] {
    final val asImply1: Imply1[p, d] = this

    // Core
    //
    def imply1[a](p: p[a]): d[a]
    def unimply1[a](d: => d[a]): p[a]
}


trait Imply1Proxy[p[+_], d[+_]] extends Imply1[p, d] with Proxy {
    override def self: Imply1[p, d]

    override def imply1[a](p: p[a]): d[a] = self.imply1(p)
    override def unimply1[a](d: => d[a]): p[a] = self.unimply1(d)

    override def asApplicative(implicit i: Applicative[p]): Applicative[d] = self.asApplicative
    override def asMonad(implicit i: Monad[p]): Monad[d] = self.asMonad
    override def asMonadCont(implicit i: MonadCont[p]): MonadCont[d] = self.asMonadCont
    override def asMonadError[e](implicit i: MonadError[e, p]): MonadError[e, d] = self.asMonadError[e]
    override def asMonadFix(implicit i: MonadFix[p]): MonadFix[d] = self.asMonadFix
    override def asMonadIO(implicit i: MonadIO[p]): MonadIO[d] = self.asMonadIO
    override def asMonadPlus(implicit i: MonadPlus[p]): MonadPlus[d] = self.asMonadPlus
    override def asMonadReader[r](implicit i: MonadReader[r, p]): MonadReader[r, d] = self.asMonadReader[r]
    override def asMonadState[s](implicit i: MonadState[s, p]): MonadState[s, d] = self.asMonadState[s]
    override def asMonadWriter[w](implicit i: MonadWriter[w, p]): MonadWriter[w, d] = self.asMonadWriter[w]
}


object Imply1 {
    def apply[p <: Kind.Function1, d <: Kind.Function1](implicit i: Imply1[p#apply, d#apply]): Imply1[p#apply, d#apply] = i
}


// Overloading weight control

private[ken] trait Imply1Instance0[p[+_], d[+_]] { outer: Imply1[p, d] =>
    implicit def asEq[z](implicit i: Eq[p[z]]): Eq[d[z]] = new Eq[d[z]] {
        private[this] type a = d[z]
        override val op_== : a => a => Bool = x => y => i.op_==(unimply1(x))(unimply1(y))
        override val op_/= : a => a => Bool = x => y => i.op_/=(unimply1(x))(unimply1(y))
    }

    implicit def asOrd[z](implicit i: Ord[p[z]]): Ord[d[z]] = new Ord[d[z]] with EqProxy[d[z]] {
        private[this] type a = d[z]
        override val self = outer.asEq(i)
        override val compare: a => a => Ordering = x => y => i.compare(unimply1(x))(unimply1(y))
        override val op_< : a => a => Bool = x => y => i.op_<(unimply1(x))(unimply1(y))
        override val op_<= : a => a => Bool = x => y => i.op_<=(unimply1(x))(unimply1(y))
        override val op_> : a => a => Bool = x => y => i.op_>(unimply1(x))(unimply1(y))
        override val op_>= : a => a => Bool = x => y => i.op_>=(unimply1(x))(unimply1(y))
        override val max: a => a => a = x => y => imply1(i.max(unimply1(x))(unimply1(y)))
        override val min: a => a => a = x => y => imply1(i.min(unimply1(x))(unimply1(y)))
    }

    implicit def asIx[z](implicit i: Ix[p[z]]): Ix[d[z]] = new Ix[d[z]] with OrdProxy[d[z]] {
        private[this] type a = d[z]
        override val self = outer.asOrd(i)
        override val range: Tuple2[a, a] => List[a] = t => List.map[p[z], a](imply1)(i.range(unimply1(t._1), unimply1(t._2)))
        override val index: Tuple2[a, a] => a => Int = t => x => i.index(unimply1(t._1), unimply1(t._2))(unimply1(x))
        override val unsafeIndex: Tuple2[a, a] => a => Int = t => x => i.unsafeIndex(unimply1(t._1), unimply1(t._2))(unimply1(x))
        override val inRange: Tuple2[a, a] => a => Bool = t => x => i.inRange(unimply1(t._1), unimply1(t._2))(unimply1(x))
        override val rangeSize: Tuple2[a, a] => Int = t => i.rangeSize(unimply1(t._1), unimply1(t._2))
        override val unsafeRangeSize: Tuple2[a, a] => Int = t => i.unsafeRangeSize(unimply1(t._1), unimply1(t._2))
    }

    implicit def asMonoid[z](implicit i: Monoid[p[z]]): Monoid[d[z]] = new Monoid[d[z]] {
        private[this] type m = d[z]
        override val mempty: m = imply1(i.mempty)
        override val mappend: m => (=> m) => m = x => y => imply1(i.mappend(unimply1(x))(unimply1(y)))
        override val mconcat: List[m] => m = xs => imply1(i.mconcat(List.map[m, p[z]](Function.!(unimply1[z]))(xs)))
    }

    implicit def asFunctor(implicit i: Functor[p]): Functor[d] = new Functor[d] {
        private[this] type f[+a] = d[a]
        override def fmap[a, b](f: a => b)(m: f[a]): f[b] = imply1 { i.fmap(f)(unimply1(m)) }
    }

    implicit def asApplicative(implicit i: Applicative[p]): Applicative[d] = new Applicative[d] with FunctorProxy[d] {
        private[this] type f[+a] = d[a]
        override val self = outer.asFunctor(i)
        override def pure[a](x: => a): f[a] = imply1 { i.pure(x) }
        override def op_<*>[a, b](x: f[a => b])(y: f[a]): f[b] = imply1 { i.op_<*>(unimply1(x))(unimply1(y)) }
        override def op_*>[a, b](x: f[a])(y: f[b]): f[b] = imply1 { i.op_*>(unimply1(x))(unimply1(y)) }
        override def op_<*[a, b](x: f[a])(y: f[b]): f[a] = imply1 { i.op_<*(unimply1(x))(unimply1(y)) }

    }

    implicit def asMonad(implicit i: Monad[p]): Monad[d] = new Monad[d] with ApplicativeProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = outer.asApplicative(i)
        override def `return`[a](x: => a): m[a] = imply1 { i.`return`(x) }
        override def op_>>=[a, b](x: m[a])(y: a => m[b]): m[b] = imply1 { i.op_>>=(unimply1(x))(a => unimply1(y(a))) }
        override def op_>>[b](x: m[_])(y: => m[b]): m[b] = imply1 { i.op_>>(unimply1[Any](x))(unimply1(y)) }
    }

    implicit def asMonadCont(implicit i: MonadCont[p]): MonadCont[d] = new MonadCont[d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = outer.asMonad(i)
        override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = imply1 {
            i.callCC { (c: a => p[b]) =>
                unimply1 { f( a => imply1(c(a)) ) }
            }
        }
    }
}

private[ken] trait Imply1Instance1[p[+_], d[+_]] extends Imply1Instance0[p, d] { outer: Imply1[p, d] =>
    implicit def asMonadError[e](implicit i: MonadError[e, p]): MonadError[e, d] = new MonadError[e, d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = outer.asMonad(i)
        override def errorClass: ErrorClass[e] = i.errorClass
        override def throwError[a](e: e): m[a] = imply1 { i.throwError(e) }
        override def catchError[a](m: m[a])(h: e => m[a]): m[a] = imply1 { i.catchError(unimply1(m))(e => unimply1(h(e))) }
    }
}

private[ken] trait Imply1Instance2[p[+_], d[+_]] extends Imply1Instance1[p, d] { outer: Imply1[p, d] =>
    implicit def asMonadFix(implicit i: MonadFix[p]): MonadFix[d] = new MonadFix[d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = outer.asMonad(i)
        override def mfix[a](f: (=> a) => m[a]): m[a] = {
            def k(a: => a): p[a] = unimply1(f(a))
            imply1 { i.mfix(k) }
        }
    }
}

private[ken] trait Imply1Instance3[p[+_], d[+_]] extends Imply1Instance2[p, d] { outer: Imply1[p, d] =>
    implicit def asMonadIO(implicit i: MonadIO[p]): MonadIO[d] = new MonadIO[d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = outer.asMonad(i)
        override def liftIO[a](io: IO[a]): m[a] = imply1 { i.liftIO(io) }
    }
}

private[ken] trait Imply1Instance4[p[+_], d[+_]] extends Imply1Instance3[p, d] { outer: Imply1[p, d] =>
    implicit def asMonadPlus(implicit i: MonadPlus[p]): MonadPlus[d] = new MonadPlus[d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = outer.asMonad(i)
        override def mzero: m[Nothing] = imply1 { i.mzero }
        override def mplus[a](x: m[a])(y: => m[a]): m[a] = imply1 { i.mplus(unimply1(x))(unimply1(y)) }
    }
}

private[ken] trait Imply1Instance5[p[+_], d[+_]] extends Imply1Instance4[p, d] { outer: Imply1[p, d] =>
    implicit def asMonadReader[r](implicit i: MonadReader[r, p]): MonadReader[r, d] = new MonadReader[r, d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = outer.asMonad(i)
        override def ask: m[r] = imply1 { i.ask }
        override def local[a](f: r => r)(m: m[a]): m[a] = imply1 { i.local(f)(unimply1(m)) }
    }
}

private[ken] trait Imply1Instance6[p[+_], d[+_]] extends Imply1Instance5[p, d] { outer: Imply1[p, d] =>
    implicit def asMonadState[s](implicit i: MonadState[s, p]): MonadState[s, d] = new MonadState[s, d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = outer.asMonad(i)
        override def get: m[s] = imply1 { i.get }
        override def put(s: s): m[Unit] = imply1 { i.put(s) }
    }
}

private[ken] trait Imply1Instance7[p[+_], d[+_]] extends Imply1Instance6[p, d] { outer: Imply1[p, d] =>
    implicit def asMonadWriter[w](implicit i: MonadWriter[w, p]): MonadWriter[w, d] = new MonadWriter[w, d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = outer.asMonad(i)
        override def monoid: Monoid[w] = i.monoid
        override def tell(x: w): m[Unit] = imply1 { i.tell(x) }
        override def listen[a](x: m[a]): m[(a, w)] = imply1 { i.listen(unimply1(x)) }
        override def pass[a](x: m[(a, w => w)]): m[a] = imply1 { i.pass(unimply1(x)) }
    }
}

private[ken] trait Imply1Instance[p[+_], d[+_]] extends Imply1Instance7[p, d] { outer: Imply1[p, d] =>
}
