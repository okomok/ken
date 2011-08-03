

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


/**
 * Weakly-typed instances
 */
trait Weak1[p[+_], d[+_]] extends TypeClass1[d] with Weak1Instance[p, d] {
    final def asWeak1: Weak1[p, apply] = this

    // Core
    //
    def wrap[a](d: => d[a]): p[a]
    def unwrap[a](p: p[a]): d[a]
}


trait Weak1Proxy[p[+_], d[+_]] extends Weak1[p, d] with Proxy {
    override def self: Weak1[p, d]

    override def wrap[a](d: => d[a]): p[a] = self.wrap(d)
    override def unwrap[a](p: p[a]): d[a] = self.unwrap(p)

    override def applicative(implicit i: Applicative[p]): Applicative[d] = self.applicative
    override def monad(implicit i: Monad[p]): Monad[d] = self.monad
    override def monadCont(implicit i: MonadCont[p]): MonadCont[d] = self.monadCont
    override def monadError[e](implicit i: MonadError[e, p]): MonadError[e, d] = self.monadError[e]
    override def monadFix(implicit i: MonadFix[p]): MonadFix[d] = self.monadFix
    override def monadIO(implicit i: MonadIO[p]): MonadIO[d] = self.monadIO
    override def monadPlus(implicit i: MonadPlus[p]): MonadPlus[d] = self.monadPlus
    override def monadReader[r](implicit i: MonadReader[r, p]): MonadReader[r, d] = self.monadReader[r]
    override def monadState[s](implicit i: MonadState[s, p]): MonadState[s, d] = self.monadState[s]
    override def monadWriter[w](implicit i: MonadWriter[w, p]): MonadWriter[w, d] = self.monadWriter[w]
}


object Weak1 {
    def apply[p[+_], d[+_]](implicit i: Weak1[p, d]): Weak1[p, d] = i
}


// Overloading weight control

private[ken] trait Weak1Instance0[p[+_], d[+_]] { this: Weak1[p, d] =>

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
}

private[ken] trait Weak1Instance1[p[+_], d[+_]] extends Weak1Instance0[p, d] { this: Weak1[p, d] =>
    implicit def monadError[e](implicit i: MonadError[e, p]): MonadError[e, d] = new MonadError[e, d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = monad(i)
        override def errorClass: ErrorClass[e] = i.errorClass
        override def throwError[a](e: e): m[a] = unwrap { i.throwError(e) }
        override def catchError[a](m: m[a])(h: e => m[a]): m[a] = unwrap { i.catchError(wrap(m))(e => wrap(h(e))) }
    }
}

private[ken] trait Weak1Instance2[p[+_], d[+_]] extends Weak1Instance1[p, d] { this: Weak1[p, d] =>
    implicit def monadFix(implicit i: MonadFix[p]): MonadFix[d] = new MonadFix[d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = monad(i)
        override def mfix[a](f: (=> a) => m[a]): m[a] = {
            def k(a: => a): p[a] = wrap(f(a))
            unwrap { i.mfix(k) }
        }
    }
}

private[ken] trait Weak1Instance3[p[+_], d[+_]] extends Weak1Instance2[p, d] { this: Weak1[p, d] =>
    implicit def monadIO(implicit i: MonadIO[p]): MonadIO[d] = new MonadIO[d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = monad(i)
        override def liftIO[a](io: IO[a]): m[a] = unwrap { i.liftIO(io) }
    }
}

private[ken] trait Weak1Instance4[p[+_], d[+_]] extends Weak1Instance3[p, d] { this: Weak1[p, d] =>
    implicit def monadPlus(implicit i: MonadPlus[p]): MonadPlus[d] = new MonadPlus[d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = monad(i)
        override def mzero: m[Nothing] = unwrap { i.mzero }
        override def mplus[a](x: m[a])(y: => m[a]): m[a] = unwrap { i.mplus(wrap(x))(wrap(y)) }
    }
}

private[ken] trait Weak1Instance5[p[+_], d[+_]] extends Weak1Instance4[p, d] { this: Weak1[p, d] =>
    implicit def monadReader[r](implicit i: MonadReader[r, p]): MonadReader[r, d] = new MonadReader[r, d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = monad(i)
        override def ask: m[r] = unwrap { i.ask }
        override def local[a](f: r => r)(m: m[a]): m[a] = unwrap { i.local(f)(wrap(m)) }
    }
}

private[ken] trait Weak1Instance6[p[+_], d[+_]] extends Weak1Instance5[p, d] { this: Weak1[p, d] =>
    implicit def monadState[s](implicit i: MonadState[s, p]): MonadState[s, d] = new MonadState[s, d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = monad(i)
        override def get: m[s] = unwrap { i.get }
        override def put(s: s): m[Unit] = unwrap { i.put(s) }
    }
}

private[ken] trait Weak1Instance7[p[+_], d[+_]] extends Weak1Instance6[p, d] { this: Weak1[p, d] =>
    implicit def monadWriter[w](implicit i: MonadWriter[w, p]): MonadWriter[w, d] = new MonadWriter[w, d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = monad(i)
        override def monoid: Monoid[w] = i.monoid
        override def tell(x: w): m[Unit] = unwrap { i.tell(x) }
        override def listen[a](x: m[a]): m[(a, w)] = unwrap { i.listen(wrap(x)) }
        override def pass[a](x: m[(a, w => w)]): m[a] = unwrap { i.pass(wrap(x)) }
    }
}

private[ken] trait Weak1Instance[p[+_], d[+_]] extends Weak1Instance7[p, d] { this: Weak1[p, d] =>
}
