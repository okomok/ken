

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


/**
 * Implements `d` instances from `p` instances.
 */
trait Imply1[p[+_], d[+_]] extends Typeclass { outer =>
    final val asImply1: Imply1[p, d] = this

    // Core
    //
    def imply1[a](p: p[a]): d[a]
    def unimply1[a](d: => d[a]): p[a]

    // Instances
    //
    def Eq[z](implicit i: Eq[p[z]]): Eq[d[z]] = new Eq[d[z]] {
        private[this] type a = d[z]
        override val op_== : a => a => Bool = x => y => i.op_==(unimply1(x))(unimply1(y))
        override val op_/= : a => a => Bool = x => y => i.op_/=(unimply1(x))(unimply1(y))
    }

    def Ord[z](implicit i: Ord[p[z]]): Ord[d[z]] = new Ord[d[z]] with EqProxy[d[z]] {
        private[this] type a = d[z]
        override val self = outer.Eq(i)
        override val compare: a => a => Ordering = x => y => i.compare(unimply1(x))(unimply1(y))
        override val op_< : a => a => Bool = x => y => i.op_<(unimply1(x))(unimply1(y))
        override val op_<= : a => a => Bool = x => y => i.op_<=(unimply1(x))(unimply1(y))
        override val op_> : a => a => Bool = x => y => i.op_>(unimply1(x))(unimply1(y))
        override val op_>= : a => a => Bool = x => y => i.op_>=(unimply1(x))(unimply1(y))
        override val max: a => a => a = x => y => imply1(i.max(unimply1(x))(unimply1(y)))
        override val min: a => a => a = x => y => imply1(i.min(unimply1(x))(unimply1(y)))
    }

    def Ix[z](implicit i: Ix[p[z]]): Ix[d[z]] = new Ix[d[z]] with OrdProxy[d[z]] {
        private[this] type a = d[z]
        override val self = outer.Ord(i)
        override val range: Tuple2[a, a] => List[a] = t => List.map[p[z], a](imply1)(i.range(unimply1(t._1), unimply1(t._2)))
        override val index: Tuple2[a, a] => a => Int = t => x => i.index(unimply1(t._1), unimply1(t._2))(unimply1(x))
        override val unsafeIndex: Tuple2[a, a] => a => Int = t => x => i.unsafeIndex(unimply1(t._1), unimply1(t._2))(unimply1(x))
        override val inRange: Tuple2[a, a] => a => Bool = t => x => i.inRange(unimply1(t._1), unimply1(t._2))(unimply1(x))
        override val rangeSize: Tuple2[a, a] => Int = t => i.rangeSize(unimply1(t._1), unimply1(t._2))
        override val unsafeRangeSize: Tuple2[a, a] => Int = t => i.unsafeRangeSize(unimply1(t._1), unimply1(t._2))
    }

    def Monoid[z](implicit i: Monoid[p[z]]): Monoid[d[z]] = new Monoid[d[z]] {
        private[this] type m = d[z]
        override val mempty: m = imply1(i.mempty)
        override val mappend: m => (=> m) => m = x => y => imply1(i.mappend(unimply1(x))(unimply1(y)))
        override val mconcat: List[m] => m = xs => imply1(i.mconcat(List.map[m, p[z]](Function.!(unimply1[z]))(xs)))
    }

    def Functor(implicit i: Functor[p]): Functor[d] = new Functor[d] {
        private[this] type f[+a] = d[a]
        override def fmap[a, b](f: a => b)(m: f[a]): f[b] = imply1 { i.fmap(f)(unimply1(m)) }
    }

    def Applicative(implicit i: Applicative[p]): Applicative[d] = new Applicative[d] with FunctorProxy[d] {
        private[this] type f[+a] = d[a]
        override val self = outer.Functor(i)
        override def pure[a](x: => a): f[a] = imply1 { i.pure(x) }
        override def op_<*>[a, b](x: f[a => b])(y: f[a]): f[b] = imply1 { i.op_<*>(unimply1(x))(unimply1(y)) }
        override def op_*>[a, b](x: f[a])(y: f[b]): f[b] = imply1 { i.op_*>(unimply1(x))(unimply1(y)) }
        override def op_<*[a, b](x: f[a])(y: f[b]): f[a] = imply1 { i.op_<*(unimply1(x))(unimply1(y)) }

    }

    def Monad(implicit i: Monad[p]): Monad[d] = new Monad[d] with ApplicativeProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = outer.Applicative(i)
        override def `return`[a](x: => a): m[a] = imply1 { i.`return`(x) }
        override def op_>>=[a, b](x: m[a])(y: a => m[b]): m[b] = imply1 { i.op_>>=(unimply1(x))(a => unimply1(y(a))) }
        override def op_>>[b](x: m[_])(y: => m[b]): m[b] = imply1 { i.op_>>(unimply1[Any](x))(unimply1(y)) }
    }

    def MonadCont(implicit i: MonadCont[p]): MonadCont[d] = new MonadCont[d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = outer.Monad(i)
        override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = imply1 {
            i.callCC { (c: a => p[b]) =>
                unimply1 { f( a => imply1(c(a)) ) }
            }
        }
    }

    def MonadError[e](implicit i: MonadError[e, p]): MonadError[e, d] = new MonadError[e, d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = outer.Monad(i)
        override def errorClass: ErrorClass[e] = i.errorClass
        override def throwError[a](e: e): m[a] = imply1 { i.throwError(e) }
        override def catchError[a](m: m[a])(h: e => m[a]): m[a] = imply1 { i.catchError(unimply1(m))(e => unimply1(h(e))) }
    }

    def MonadFix(implicit i: MonadFix[p]): MonadFix[d] = new MonadFix[d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = outer.Monad(i)
        override def mfix[a](f: (=> a) => m[a]): m[a] = {
            def k(a: => a): p[a] = unimply1(f(a))
            imply1 { i.mfix(k) }
        }
    }

    def MonadIO(implicit i: MonadIO[p]): MonadIO[d] = new MonadIO[d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = outer.Monad(i)
        override def liftIO[a](io: IO[a]): m[a] = imply1 { i.liftIO(io) }
    }

    def MonadPlus(implicit i: MonadPlus[p]): MonadPlus[d] = new MonadPlus[d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = outer.Monad(i)
        override def mzero: m[Nothing] = imply1 { i.mzero }
        override def mplus[a](x: m[a])(y: => m[a]): m[a] = imply1 { i.mplus(unimply1(x))(unimply1(y)) }
    }

    def MonadReader[r](implicit i: MonadReader[r, p]): MonadReader[r, d] = new MonadReader[r, d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = outer.Monad(i)
        override def ask: m[r] = imply1 { i.ask }
        override def local[a](f: r => r)(m: m[a]): m[a] = imply1 { i.local(f)(unimply1(m)) }
    }

    def MonadState[s](implicit i: MonadState[s, p]): MonadState[s, d] = new MonadState[s, d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = outer.Monad(i)
        override def get: m[s] = imply1 { i.get }
        override def put(s: s): m[Unit] = imply1 { i.put(s) }
    }

    def MonadWriter[w](implicit i: MonadWriter[w, p]): MonadWriter[w, d] = new MonadWriter[w, d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = outer.Monad(i)
        override def monoid: Monoid[w] = i.monoid
        override def tell(x: w): m[Unit] = imply1 { i.tell(x) }
        override def listen[a](x: m[a]): m[(a, w)] = imply1 { i.listen(unimply1(x)) }
        override def pass[a](x: m[(a, w => w)]): m[a] = imply1 { i.pass(unimply1(x)) }
    }
}


trait Imply1Proxy[p[+_], d[+_]] extends Imply1[p, d] with Proxy {
    override def self: Imply1[p, d]

    override def imply1[a](p: p[a]): d[a] = self.imply1(p)
    override def unimply1[a](d: => d[a]): p[a] = self.unimply1(d)

    override def Eq[z](implicit i: Eq[p[z]]): Eq[d[z]] = self.Eq(i)
    override def Ord[z](implicit i: Ord[p[z]]): Ord[d[z]] = self.Ord(i)
    override def Ix[z](implicit i: Ix[p[z]]): Ix[d[z]] = self.Ix(i)
    override def Monoid[z](implicit i: Monoid[p[z]]): Monoid[d[z]] = self.Monoid(i)

    override def Functor(implicit i: Functor[p]): Functor[d] = self.Functor(i)
    override def Applicative(implicit i: Applicative[p]): Applicative[d] = self.Applicative(i)
    override def Monad(implicit i: Monad[p]): Monad[d] = self.Monad(i)
    override def MonadCont(implicit i: MonadCont[p]): MonadCont[d] = self.MonadCont(i)
    override def MonadError[e](implicit i: MonadError[e, p]): MonadError[e, d] = self.MonadError(i)
    override def MonadFix(implicit i: MonadFix[p]): MonadFix[d] = self.MonadFix(i)
    override def MonadIO(implicit i: MonadIO[p]): MonadIO[d] = self.MonadIO(i)
    override def MonadPlus(implicit i: MonadPlus[p]): MonadPlus[d] = self.MonadPlus(i)
    override def MonadReader[r](implicit i: MonadReader[r, p]): MonadReader[r, d] = self.MonadReader(i)
    override def MonadState[s](implicit i: MonadState[s, p]): MonadState[s, d] = self.MonadState(i)
    override def MonadWriter[w](implicit i: MonadWriter[w, p]): MonadWriter[w, d] = self.MonadWriter(i)
}


object Imply1 {
    def apply[p <: Kind.Function1, d <: Kind.Function1](implicit i: Imply1[p#apply, d#apply]): Imply1[p#apply, d#apply] = i
}
