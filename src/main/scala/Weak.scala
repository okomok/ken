

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


/**
 * Weakly-typed monads
 */
trait Weak[p[+_], d[+_]] extends Klass {
    type apply[+a] = d[a]

    def wrap[a](d: => d[a]): p[a]
    def unwrap[a](p: p[a]): d[a]

    implicit def functor(implicit i: Functor[p]): Functor[d] = new Functor[d] {
        private[this] type f[+a] = d[a]
        override def fmap[a, b](f: a => b)(m: f[a]): f[b] = unwrap { i.fmap(f)(wrap(m)) }
    }

    implicit def applicative(implicit i: Applicative[p]): Applicative[d] = new Applicative[d] with FunctorProxy[d] {
        private[this] type f[+a] = d[a]
        override val self = functor(i)
        override def pure[a](x: => a): f[a] = unwrap { i.pure(x) }
        override def op_<*>[a, b](x: f[a => b])(y: f[a]): f[b] = unwrap { i.op_<*>(wrap(x))(wrap(y)) }
        override def op_*>[a, b](x: f[a])(y: f[b]): f[b] = unwrap { i.op_*>(wrap(x))(wrap(y)) }
        override def op_<*[a, b](x: f[a])(y: f[b]): f[a] = unwrap { i.op_<*(wrap(x))(wrap(y)) }

    }

    implicit def monad(implicit i: Monad[p]): Monad[d] = new Monad[d] with ApplicativeProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = applicative(i)
        override def `return`[a](x: => a): m[a] = unwrap { i.`return`(x) }
        override def op_>>=[a, b](x: m[a])(y: a => m[b]): m[b] = unwrap { i.op_>>=(wrap(x))(a => wrap(y(a))) }
        override def op_>>[b](x: m[_])(y: => m[b]): m[b] = unwrap { i.op_>>(wrap[Any](x))(wrap(y)) }
    }

    implicit def monadCont(implicit i: MonadCont[p]): MonadCont[d] = new MonadCont[d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = monad(i)
        override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = unwrap {
            i.callCC { (c: a => p[b]) =>
                wrap { f( a => unwrap(c(a)) ) }
            }
        }
    }

    implicit def monadError[e](implicit i: MonadError[e, p]): MonadError[e, d] = new MonadError[e, d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = monad(i)
        override def errorClass: ErrorClass[e] = i.errorClass
        override def throwError[a](e: e): m[a] = unwrap { i.throwError(e) }
        override def catchError[a](m: m[a])(h: e => m[a]): m[a] = unwrap { i.catchError(wrap(m))(e => wrap(h(e))) }
    }

    implicit def monadFix(implicit i: MonadFix[p]): MonadFix[d] = new MonadFix[d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = monad(i)
        override def mfix[a](f: (=> a) => m[a]): m[a] = {
            def k(a: => a): p[a] = wrap(f(a))
            unwrap { i.mfix(k) }
        }
    }

    implicit def monadIO(implicit i: MonadIO[p]): MonadIO[d] = new MonadIO[d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = monad(i)
        override def liftIO[a](io: IO[a]): m[a] = unwrap { i.liftIO(io) }
    }

    implicit def monadPlus(implicit i: MonadPlus[p]): MonadPlus[d] = new MonadPlus[d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = monad(i)
        override def mzero: m[Nothing] = unwrap { i.mzero }
        override def mplus[a](x: m[a])(y: => m[a]): m[a] = unwrap { i.mplus(wrap(x))(wrap(y)) }
    }

    implicit def monadReader[r](implicit i: MonadReader[r, p]): MonadReader[r, d] = new MonadReader[r, d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = monad(i)
        override def ask: m[r] = unwrap { i.ask }
        override def local[a](f: r => r)(m: m[a]): m[a] = unwrap { i.local(f)(wrap(m)) }
    }

    implicit def monadState[s](implicit i: MonadState[s, p]): MonadState[s, d] = new MonadState[s, d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = monad(i)
        override def get: m[s] = unwrap { i.get }
        override def put(s: s): m[Unit] = unwrap { i.put(s) }
    }

    implicit def monadWriter[w](implicit i: MonadWriter[w, p]): MonadWriter[w, d] = new MonadWriter[w, d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = monad(i)
        override def monoid: Monoid[w] = i.monoid
        override def tell(x: w): m[Unit] = unwrap { i.tell(x) }
        override def listen[a](x: m[a]): m[(a, w)] = unwrap { i.listen(wrap(x)) }
        override def pass[a](x: m[(a, w => w)]): m[a] = unwrap { i.pass(wrap(x)) }
    }
}


trait WeakProxy[p[+_], d[+_]] extends Weak[p, d] with Proxy {
    override def self: Weak[p, d]

    override def wrap[a](d: => d[a]): p[a] = self.wrap(d)
    override def unwrap[a](p: p[a]): d[a] = self.unwrap(p)

    override implicit def applicative(implicit i: Applicative[p]): Applicative[d] = self.applicative
    override implicit def monad(implicit i: Monad[p]): Monad[d] = self.monad
    override implicit def monadCont(implicit i: MonadCont[p]): MonadCont[d] = self.monadCont
    override implicit def monadError[e](implicit i: MonadError[e, p]): MonadError[e, d] = self.monadError[e]
    override implicit def monadFix(implicit i: MonadFix[p]): MonadFix[d] = self.monadFix
    override implicit def monadIO(implicit i: MonadIO[p]): MonadIO[d] = self.monadIO
    override implicit def monadPlus(implicit i: MonadPlus[p]): MonadPlus[d] = self.monadPlus
    override implicit def monadReader[r](implicit i: MonadReader[r, p]): MonadReader[r, d] = self.monadReader[r]
    override implicit def monadState[s](implicit i: MonadState[s, p]): MonadState[s, d] = self.monadState[s]
    override implicit def monadWriter[w](implicit i: MonadWriter[w, p]): MonadWriter[w, d] = self.monadWriter[w]
}


object Weak {
    def apply[p[+_], d[+_]](implicit i: Weak[p, d]): Weak[p, d] = i
}
