

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


/**
 * Weakly-typed instances
 */
trait Weak1[p[+_], d[+_]] extends TypeClass1[d] with Weak1Instance[p, d] {
    final val asWeak1: Weak1[p, apply] = this

    // Core
    //
    def wrap[a](d: => d[a]): p[a]
    def unwrap[a](p: p[a]): d[a]
}


trait Weak1Proxy[p[+_], d[+_]] extends Weak1[p, d] with Proxy {
    override def self: Weak1[p, d]

    override def wrap[a](d: => d[a]): p[a] = self.wrap(d)
    override def unwrap[a](p: p[a]): d[a] = self.unwrap(p)

    override def asApplicative(implicit i: Applicative[p]): Applicative[d] = self.asApplicative
    override def asMonad(implicit i: Monad[p]): Monad[d] = self.asMonad
    override def asMonadCont(implicit i: MonadCont[p]): MonadCont[d] = self.asMonadCont
    override def asMonadError[e](implicit i: MonadError[e, p]): MonadError[e, d] = self.asMonadError[e]
    override def asMonadFix(implicit i: MonadFix[p]): MonadFix[d] = self.asMonadFix
    override def asMonadIO(implicit i: MonadIO[p]): MonadIO[d] = self.asMonadIO
    override def asMonadPlus(implicit i: MonadPlus[p]): MonadPlus[d] = self.asMonadPlus
    override def monadReader[r](implicit i: MonadReader[r, p]): MonadReader[r, d] = self.monadReader[r]
    override def asMonadState[s](implicit i: MonadState[s, p]): MonadState[s, d] = self.asMonadState[s]
    override def asMonadWriter[w](implicit i: MonadWriter[w, p]): MonadWriter[w, d] = self.asMonadWriter[w]
}


object Weak1 {
    def apply[p[+_], d[+_]](implicit i: Weak1[p, d]): Weak1[p, d] = i
}


// Overloading weight control

private[ken] trait Weak1Instance0[p[+_], d[+_]] { outer: Weak1[p, d] =>

    implicit def asFunctor(implicit i: Functor[p]): Functor[d] = new Functor[d] {
        private[this] type f[+a] = d[a]
        override def fmap[a, b](f: a => b)(m: f[a]): f[b] = unwrap { i.fmap(f)(wrap(m)) }
    }

    implicit def asApplicative(implicit i: Applicative[p]): Applicative[d] = new Applicative[d] with FunctorProxy[d] {
        private[this] type f[+a] = d[a]
        override val self = outer.asFunctor(i)
        override def pure[a](x: => a): f[a] = unwrap { i.pure(x) }
        override def op_<*>[a, b](x: f[a => b])(y: f[a]): f[b] = unwrap { i.op_<*>(wrap(x))(wrap(y)) }
        override def op_*>[a, b](x: f[a])(y: f[b]): f[b] = unwrap { i.op_*>(wrap(x))(wrap(y)) }
        override def op_<*[a, b](x: f[a])(y: f[b]): f[a] = unwrap { i.op_<*(wrap(x))(wrap(y)) }

    }

    implicit def asMonad(implicit i: Monad[p]): Monad[d] = new Monad[d] with ApplicativeProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = outer.asApplicative(i)
        override def `return`[a](x: => a): m[a] = unwrap { i.`return`(x) }
        override def op_>>=[a, b](x: m[a])(y: a => m[b]): m[b] = unwrap { i.op_>>=(wrap(x))(a => wrap(y(a))) }
        override def op_>>[b](x: m[_])(y: => m[b]): m[b] = unwrap { i.op_>>(wrap[Any](x))(wrap(y)) }
    }

    implicit def asMonadCont(implicit i: MonadCont[p]): MonadCont[d] = new MonadCont[d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = outer.asMonad(i)
        override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = unwrap {
            i.callCC { (c: a => p[b]) =>
                wrap { f( a => unwrap(c(a)) ) }
            }
        }
    }
}

private[ken] trait Weak1Instance1[p[+_], d[+_]] extends Weak1Instance0[p, d] { outer: Weak1[p, d] =>
    implicit def asMonadError[e](implicit i: MonadError[e, p]): MonadError[e, d] = new MonadError[e, d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = outer.asMonad(i)
        override def errorClass: ErrorClass[e] = i.errorClass
        override def throwError[a](e: e): m[a] = unwrap { i.throwError(e) }
        override def catchError[a](m: m[a])(h: e => m[a]): m[a] = unwrap { i.catchError(wrap(m))(e => wrap(h(e))) }
    }
}

private[ken] trait Weak1Instance2[p[+_], d[+_]] extends Weak1Instance1[p, d] { outer: Weak1[p, d] =>
    implicit def asMonadFix(implicit i: MonadFix[p]): MonadFix[d] = new MonadFix[d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = outer.asMonad(i)
        override def mfix[a](f: (=> a) => m[a]): m[a] = {
            def k(a: => a): p[a] = wrap(f(a))
            unwrap { i.mfix(k) }
        }
    }
}

private[ken] trait Weak1Instance3[p[+_], d[+_]] extends Weak1Instance2[p, d] { outer: Weak1[p, d] =>
    implicit def asMonadIO(implicit i: MonadIO[p]): MonadIO[d] = new MonadIO[d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = outer.asMonad(i)
        override def liftIO[a](io: IO[a]): m[a] = unwrap { i.liftIO(io) }
    }
}

private[ken] trait Weak1Instance4[p[+_], d[+_]] extends Weak1Instance3[p, d] { outer: Weak1[p, d] =>
    implicit def asMonadPlus(implicit i: MonadPlus[p]): MonadPlus[d] = new MonadPlus[d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = outer.asMonad(i)
        override def mzero: m[Nothing] = unwrap { i.mzero }
        override def mplus[a](x: m[a])(y: => m[a]): m[a] = unwrap { i.mplus(wrap(x))(wrap(y)) }
    }
}

private[ken] trait Weak1Instance5[p[+_], d[+_]] extends Weak1Instance4[p, d] { outer: Weak1[p, d] =>
    implicit def monadReader[r](implicit i: MonadReader[r, p]): MonadReader[r, d] = new MonadReader[r, d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = outer.asMonad(i)
        override def ask: m[r] = unwrap { i.ask }
        override def local[a](f: r => r)(m: m[a]): m[a] = unwrap { i.local(f)(wrap(m)) }
    }
}

private[ken] trait Weak1Instance6[p[+_], d[+_]] extends Weak1Instance5[p, d] { outer: Weak1[p, d] =>
    implicit def asMonadState[s](implicit i: MonadState[s, p]): MonadState[s, d] = new MonadState[s, d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = outer.asMonad(i)
        override def get: m[s] = unwrap { i.get }
        override def put(s: s): m[Unit] = unwrap { i.put(s) }
    }
}

private[ken] trait Weak1Instance7[p[+_], d[+_]] extends Weak1Instance6[p, d] { outer: Weak1[p, d] =>
    implicit def asMonadWriter[w](implicit i: MonadWriter[w, p]): MonadWriter[w, d] = new MonadWriter[w, d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = outer.asMonad(i)
        override def asMonoid: Monoid[w] = i.asMonoid
        override def tell(x: w): m[Unit] = unwrap { i.tell(x) }
        override def listen[a](x: m[a]): m[(a, w)] = unwrap { i.listen(wrap(x)) }
        override def pass[a](x: m[(a, w => w)]): m[a] = unwrap { i.pass(wrap(x)) }
    }
}

private[ken] trait Weak1Instance[p[+_], d[+_]] extends Weak1Instance7[p, d] { outer: Weak1[p, d] =>
}
