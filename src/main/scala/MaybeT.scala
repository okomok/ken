

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


final class _MaybeTs[n[+_]](val inner: Monad[n]) {
    private[this] implicit def innerFor[a](x: n[a]): inner.For[a] = inner.`for`(x)
    private[this] implicit def innerInfix_>>=[a](x: n[a]): inner.Infix_>>=[a] = inner.>>=(x)

    sealed abstract class _MaybeT[+a] extends Identity[n[Maybe[a]]]

    object _MaybeT extends Instances {
        def apply[a](rep: n[Maybe[a]]): _MaybeT[a] = new _MaybeT[a] {
            override def run: n[Maybe[a]] = rep
        }

        implicit def from[a](n: Identity[n[Maybe[a]]]): _MaybeT[a] = _MaybeT { n.run }

        def run[a](n: _MaybeT[a]): n[Maybe[a]] = n.run

        def map[m[+_], a, b](f: n[Maybe[a]] => m[Maybe[b]])(n: _MaybeT[a]): Identity[m[Maybe[b]]] = Identity { f(run(n)) }
    }

    sealed abstract class LowPriorityInstances { this: _MaybeT.type =>
        implicit val monad: MonadPlus[_MaybeT] with inner.Trans[_MaybeT] = new MonadPlus[_MaybeT] with inner.Trans[_MaybeT] {
            // Monad
            private[this] type m[+a] = _MaybeT[a]
            override def `return`[a](a: => a): m[a] = _MaybeT { inner.`return`(Just(a).up) }
            override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = _MaybeT {
                run(m) >>= {
                    case Nothing => inner.`return`(Nothing.of[b])
                    case Just(v) => run(k(v))
                }
            }
            // MonadPlus
            override def mzero: m[Nothing] = _MaybeT { inner.`return`(Nothing) }
            override def mplus[a](x: m[a])(y: => m[a]): m[a] = _MaybeT {
                val runx = run(x)
                runx >>= {
                    case Nothing => run(y)
                    case Just(_) => runx
                }
            }
            // Trans
            override def lift[a](n: n[a]): m[a] = _MaybeT {
                for { a <- n } yield Just(a)
            }
        }
    }

    sealed abstract class Instances extends LowPriorityInstances { this: _MaybeT.type =>
        implicit def monadIO(implicit i: MonadIO[n]): MonadIO[_MaybeT] =
            new MonadIO[_MaybeT] with MonadProxy[_MaybeT]
        {
            private[this] type m[+a] = _MaybeT[a]
            override def self = monad
            override def liftIO[a](io: IO[a]): m[a] = self.lift(i.liftIO(io))
        }

        implicit def monadCont(implicit i: MonadCont[n]): MonadCont[_MaybeT] =
            new MonadCont[_MaybeT] with MonadProxy[_MaybeT]
        {
            private[this] type m[+a] = _MaybeT[a]
            override val self = monad
            override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = _MaybeT {
                i.callCC { (c: Maybe[a] => n[Maybe[b]]) =>
                    run( f( a => _MaybeT { c(Just(a)) } ) )
                }
            }
        }

        implicit def monadError[e](implicit i: MonadError[e, n]): MonadError[e, _MaybeT] =
            new MonadError[e, _MaybeT] with MonadProxy[_MaybeT]
        {
            private[this] type m[+a] = _MaybeT[a]
            override val self = monad
            override def errorClass: ErrorClass[e] = i.errorClass
            override def throwError[a](e: e): m[a] = self.lift(i.throwError(e))
            override def catchError[a](m: m[a])(h: e => m[a]): m[a] = _MaybeT {
                i.catchError(run(m)) { e => run(h(e)) }
            }
        }

        implicit def monadReader[r](implicit i: MonadReader[r, n]): MonadReader[r, _MaybeT] =
            new MonadReader[r, _MaybeT] with MonadProxy[_MaybeT]
        {
            private[this] type m[+a] = _MaybeT[a]
            override val self = monad
            override def ask: m[r] = self.lift(i.ask)
            override def local[a](f: r => r)(m: m[a]): m[a] = _MaybeT { i.local(f)(run(m)) }
        }

        implicit def monadState[s](implicit i: MonadState[s, n]): MonadState[s, _MaybeT] =
            new MonadState[s, _MaybeT] with MonadProxy[_MaybeT]
        {
            private[this] type m[+a] = _MaybeT[a]
            override val self = monad
            override def get: m[s] = self.lift(i.get)
            override def put(s: s): m[Unit] = self.lift(i.put(s))
        }

        implicit val weak: Weak[_MaybeT, ({type d[+a] = n[Maybe[a]]})#d] =
            new Weak[_MaybeT, ({type d[+a] = n[Maybe[a]]})#d]
        {
            private[this] type p[+a] = _MaybeT[a]
            private[this] type d[+a] = n[Maybe[a]]
            override def wrap[a](d: => d[a]): p[a] = _MaybeT(d)
            override def unwrap[a](p: p[a]): d[a] = run(p)
        }
    }
}
