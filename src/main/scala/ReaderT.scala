

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


final class _ReaderTs[n[+_]](val inner: Monad[n]) {
    private[this] implicit def innerFor[a](x: n[a]): inner.For[a] = inner.`for`(x)

    sealed abstract class _ReaderT[r, +a] extends Wrap[r => n[a]]

    object _ReaderT extends Instances {
        def apply[r, a](rep: r => n[a]): _ReaderT[r, a] = new _ReaderT[r, a] {
            override def run: r => n[a] = rep
        }

        implicit def from[r, a](n: Wrap[r => n[a]]): _ReaderT[r, a] = _ReaderT { n.run }

        def run[r, a](n: _ReaderT[r, a]): r => n[a] = n.run

        def map[r, m[+_], a, b](f: n[a] => m[b])(n: _ReaderT[r, a]): Wrap[r => m[b]] = Wrap { f compose run(n) }

        def `with`[r, r_, a](f: r_ => r)(n: _ReaderT[r, a]): _ReaderT[r_, a] = _ReaderT { run(n) compose f }
    }

    trait LowPriorityImplicits { this: _ReaderT.type =>
        implicit def monad[r]: MonadReader[r, ({type m[+a] = _ReaderT[r, a]})#m] with inner.Trans[({type m[+a] = _ReaderT[r, a]})#m] =
            new MonadReader[r, ({type m[+a] = _ReaderT[r, a]})#m] with inner.Trans[({type m[+a] = _ReaderT[r, a]})#m]
        {
            // Functor
            private[this] type f[+a] = _ReaderT[r, a]
            override def fmap[a, b](f: a => b)(m: f[a]): f[b] = _ReaderT { r =>
                for { a <- run(m)(r) } yield f(a)
            }
            // Monad
            private[this] type m[+a] = f[a]
            override def `return`[a](a: => a): m[a] = _ReaderT { r => inner.`return`(a) }
            override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = _ReaderT { r =>
                for { a <- run(m)(r); * <- run(k(a))(r) } yield *
            }
            // MonadReader
            override def ask: m[r] = _ReaderT { r => inner.`return`(r) }
            override def local[a](f: r => r)(m: m[a]): m[a] = _ReaderT { r => run(m)(f(r)) }
            // Trans
            override def lift[a](n: n[a]): m[a] = _ReaderT { _ => n }
        }
    }

    trait Instances extends LowPriorityImplicits { this: _ReaderT.type =>
        implicit def monadPlus[r](implicit i: MonadPlus[n]): MonadPlus[({type m[+a] = _ReaderT[r, a]})#m] =
            new MonadPlus[({type m[+a] = _ReaderT[r, a]})#m] with MonadProxy[({type m[+a] = _ReaderT[r, a]})#m]
        {
            private[this] type m[+a] = _ReaderT[r, a]
            override val self = monad[r]
            override def mzero: m[Nothing] = _ReaderT { _ => i.mzero }
            override def mplus[a](m: m[a])(n: => m[a]): m[a] = _ReaderT { r => i.mplus(run(m)(r))(run(n)(r)) }
        }

        implicit def monadFix[r](implicit i: MonadFix[n]): MonadFix[({type m[+a] = _ReaderT[r, a]})#m] =
            new MonadFix[({type m[+a] = _ReaderT[r, a]})#m] with MonadProxy[({type m[+a] = _ReaderT[r, a]})#m]
        {
            private[this] type m[+a] = _ReaderT[r, a]
            override val self = monad[r]
            override def mfix[a](f: (=> a) => m[a]): m[a] = _ReaderT { r =>
                def k(a: => a) = run(f(a))(r)
                i.mfix(k)
            }
        }

        implicit def monadIO[r](implicit i: MonadIO[n]): MonadIO[({type m[+a] = _ReaderT[r, a]})#m] =
            new MonadIO[({type m[+a] = _ReaderT[r, a]})#m] with MonadProxy[({type m[+a] = _ReaderT[r, a]})#m]
        {
            private[this] type m[+a] = _ReaderT[r, a]
            override val self = monad[r]
            override def liftIO[a](io: IO[a]): m[a] = self.lift(i.liftIO(io))
        }

        implicit def monadCont[r](implicit i: MonadCont[n]): MonadCont[({type m[+a] = _ReaderT[r, a]})#m] =
            new MonadCont[({type m[+a] = _ReaderT[r, a]})#m] with MonadProxy[({type m[+a] = _ReaderT[r, a]})#m]
        {
            private[this] type m[+a] = _ReaderT[r, a]
            override val self = monad[r]
            override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = _ReaderT { r =>
                i.callCC { (c: a => n[b]) =>
                    run( f( a => _ReaderT { s_ => c(a) } ) )(r)
                }
            }
        }

        implicit def monadError[r, e](implicit i: MonadError[e, n]): MonadError[e, ({type m[+a] = _ReaderT[r, a]})#m] =
            new MonadError[e, ({type m[+a] = _ReaderT[r, a]})#m] with MonadProxy[({type m[+a] = _ReaderT[r, a]})#m]
        {
            private[this] type m[+a] = _ReaderT[r, a]
            override val self = monad[r]
            override def errorClass: ErrorClass[e] = i.errorClass
            override def throwError[a](e: e): m[a] = self.lift(i.throwError(e))
            override def catchError[a](m: m[a])(h: e => m[a]): m[a] = _ReaderT { r =>
                i.catchError(run(m)(r)) { e => run(h(e))(r) }
            }
        }

        implicit def monadState[r, s](implicit i: MonadState[s, n]): MonadState[s, ({type m[+a] = _ReaderT[r, a]})#m] =
            new MonadState[s, ({type m[+a] = _ReaderT[r, a]})#m] with MonadProxy[({type m[+a] = _ReaderT[r, a]})#m]
        {
            private[this] type m[+a] = _ReaderT[r, a]
            override val self = monad[r]
            override def get: m[s] = self.lift(i.get)
            override def put(s: s): m[Unit] = self.lift(i.put(s))
        }

        implicit def monadWriter[r, w](implicit i: MonadWriter[w, n]): MonadWriter[w, ({type m[+a] = _ReaderT[r, a]})#m] =
            new MonadWriter[w, ({type m[+a] = _ReaderT[r, a]})#m] with MonadProxy[({type m[+a] = _ReaderT[r, a]})#m]
        {
            private[this] type m[+a] = _ReaderT[r, a]
            override val self = monad[r]
            override def monoid: Monoid[w] = i.monoid
            override def tell(x: w): m[Unit] = self.lift(i.tell(x))
            override def listen[a](m: m[a]): m[(a, w)] = _ReaderT { w => i.listen(run(m)(w)) }
            override def pass[a](m: m[(a, w => w)]): m[a] = _ReaderT { w => i.pass(run(m)(w)) }
        }
    }
}
