

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
    def imply[a](p: p[a]): d[a]
    def unimply[a](d: => d[a]): p[a]
}


trait Imply1Proxy[p[+_], d[+_]] extends Imply1[p, d] with Proxy {
    override def self: Imply1[p, d]

    override def imply[a](p: p[a]): d[a] = self.imply(p)
    override def unimply[a](d: => d[a]): p[a] = self.unimply(d)

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
    implicit def asFunctor(implicit i: Functor[p]): Functor[d] = new Functor[d] {
        private[this] type f[+a] = d[a]
        override def fmap[a, b](f: a => b)(m: f[a]): f[b] = imply { i.fmap(f)(unimply(m)) }
    }

    implicit def asApplicative(implicit i: Applicative[p]): Applicative[d] = new Applicative[d] with FunctorProxy[d] {
        private[this] type f[+a] = d[a]
        override val self = outer.asFunctor(i)
        override def pure[a](x: => a): f[a] = imply { i.pure(x) }
        override def op_<*>[a, b](x: f[a => b])(y: f[a]): f[b] = imply { i.op_<*>(unimply(x))(unimply(y)) }
        override def op_*>[a, b](x: f[a])(y: f[b]): f[b] = imply { i.op_*>(unimply(x))(unimply(y)) }
        override def op_<*[a, b](x: f[a])(y: f[b]): f[a] = imply { i.op_<*(unimply(x))(unimply(y)) }

    }

    implicit def asMonad(implicit i: Monad[p]): Monad[d] = new Monad[d] with ApplicativeProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = outer.asApplicative(i)
        override def `return`[a](x: => a): m[a] = imply { i.`return`(x) }
        override def op_>>=[a, b](x: m[a])(y: a => m[b]): m[b] = imply { i.op_>>=(unimply(x))(a => unimply(y(a))) }
        override def op_>>[b](x: m[_])(y: => m[b]): m[b] = imply { i.op_>>(unimply[Any](x))(unimply(y)) }
    }

    implicit def asMonadCont(implicit i: MonadCont[p]): MonadCont[d] = new MonadCont[d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = outer.asMonad(i)
        override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = imply {
            i.callCC { (c: a => p[b]) =>
                unimply { f( a => imply(c(a)) ) }
            }
        }
    }
}

private[ken] trait Imply1Instance1[p[+_], d[+_]] extends Imply1Instance0[p, d] { outer: Imply1[p, d] =>
    implicit def asMonadError[e](implicit i: MonadError[e, p]): MonadError[e, d] = new MonadError[e, d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = outer.asMonad(i)
        override def errorClass: ErrorClass[e] = i.errorClass
        override def throwError[a](e: e): m[a] = imply { i.throwError(e) }
        override def catchError[a](m: m[a])(h: e => m[a]): m[a] = imply { i.catchError(unimply(m))(e => unimply(h(e))) }
    }
}

private[ken] trait Imply1Instance2[p[+_], d[+_]] extends Imply1Instance1[p, d] { outer: Imply1[p, d] =>
    implicit def asMonadFix(implicit i: MonadFix[p]): MonadFix[d] = new MonadFix[d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = outer.asMonad(i)
        override def mfix[a](f: (=> a) => m[a]): m[a] = {
            def k(a: => a): p[a] = unimply(f(a))
            imply { i.mfix(k) }
        }
    }
}

private[ken] trait Imply1Instance3[p[+_], d[+_]] extends Imply1Instance2[p, d] { outer: Imply1[p, d] =>
    implicit def asMonadIO(implicit i: MonadIO[p]): MonadIO[d] = new MonadIO[d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = outer.asMonad(i)
        override def liftIO[a](io: IO[a]): m[a] = imply { i.liftIO(io) }
    }
}

private[ken] trait Imply1Instance4[p[+_], d[+_]] extends Imply1Instance3[p, d] { outer: Imply1[p, d] =>
    implicit def asMonadPlus(implicit i: MonadPlus[p]): MonadPlus[d] = new MonadPlus[d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = outer.asMonad(i)
        override def mzero: m[Nothing] = imply { i.mzero }
        override def mplus[a](x: m[a])(y: => m[a]): m[a] = imply { i.mplus(unimply(x))(unimply(y)) }
    }
}

private[ken] trait Imply1Instance5[p[+_], d[+_]] extends Imply1Instance4[p, d] { outer: Imply1[p, d] =>
    implicit def asMonadReader[r](implicit i: MonadReader[r, p]): MonadReader[r, d] = new MonadReader[r, d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = outer.asMonad(i)
        override def ask: m[r] = imply { i.ask }
        override def local[a](f: r => r)(m: m[a]): m[a] = imply { i.local(f)(unimply(m)) }
    }
}

private[ken] trait Imply1Instance6[p[+_], d[+_]] extends Imply1Instance5[p, d] { outer: Imply1[p, d] =>
    implicit def asMonadState[s](implicit i: MonadState[s, p]): MonadState[s, d] = new MonadState[s, d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = outer.asMonad(i)
        override def get: m[s] = imply { i.get }
        override def put(s: s): m[Unit] = imply { i.put(s) }
    }
}

private[ken] trait Imply1Instance7[p[+_], d[+_]] extends Imply1Instance6[p, d] { outer: Imply1[p, d] =>
    implicit def asMonadWriter[w](implicit i: MonadWriter[w, p]): MonadWriter[w, d] = new MonadWriter[w, d] with MonadProxy[d] {
        private[this] type m[+a] = d[a]
        override val self = outer.asMonad(i)
        override def monoid: Monoid[w] = i.monoid
        override def tell(x: w): m[Unit] = imply { i.tell(x) }
        override def listen[a](x: m[a]): m[(a, w)] = imply { i.listen(unimply(x)) }
        override def pass[a](x: m[(a, w => w)]): m[a] = imply { i.pass(unimply(x)) }
    }
}

private[ken] trait Imply1Instance[p[+_], d[+_]] extends Imply1Instance7[p, d] { outer: Imply1[p, d] =>
}
