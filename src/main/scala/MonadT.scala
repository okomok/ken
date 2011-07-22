

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


import MonadT._


/**
 * Monad Transformers
 */
final class MonadT[n[+_]](implicit val inner: Monad[n]) {

// Trans
    trait Trans[m[+_]] {
        def lift[a](n: n[a]): m[a]
    }

    object Trans {
        def apply[m[+_]](implicit i: Trans[m]): Trans[m] = i

        implicit val trivial: Trans[n] = new Trans[n] {
            override def lift[a](n: n[a]): n[a] = n
        }
    }

// Hidden name resurrection
    private[ken] trait InnerMonadMethod {
        protected[this] implicit def innerFor[a](x: n[a]): inner.For[a] = inner.`for`(x)
        protected[this] implicit def inner_>>=[a](x: n[a]): inner.Infix_>>=[a] = inner.>>=(x)
    }

// ErrorT
    type ErrorT[e, +a] = n[Either[e, a]]

    trait ErrorTLowPriorityImplicits { this: ErrorT.type =>
        implicit def monad[e](implicit i: ErrorClass[e]): MonadPlus[({type m[+a] = ErrorT[e, a]})#m] with MonadError[e, ({type m[+a] = ErrorT[e, a]})#m] with Trans[({type m[+a] = ErrorT[e, a]})#m] =
            new MonadPlus[({type m[+a] = ErrorT[e, a]})#m] with MonadError[e, ({type m[+a] = ErrorT[e, a]})#m] with Trans[({type m[+a] = ErrorT[e, a]})#m]
        {
            // Functor
            private[this] type f[+a] = ErrorT[e, a]
            override def fmap[a, b](f: a => b)(m: f[a]): f[b] = {
                import inner.`for` // ambiguity buster
                for { a <- m } yield (a match {
                    case Left(l) => Left(l)
                    case Right(r) => Right(f(r))
                })
            }
            // Monad
            private[this] type m[+a] = f[a]
            override def `return`[a](a: a): m[a] = inner.`return`(Right(a))
            override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = {
                import inner.`for`
                for {
                    a <- m
                    * <- a match {
                        case Left(l) => inner.`return`(Left(l))
                        case Right(r) => k(r)
                    }
                } yield *
            }
            // MonadPlus
            override def mzero: m[Nothing] = inner.`return`(Left(i.noMsg))
            override def mplus[a](m: m[a])(n: => m[a]): m[a] = {
                import inner.`for`
                for {
                    a <- m
                    * <- a match {
                        case Left(_) => n
                        case Right(r) => inner.`return`(Right(r))
                    }
                } yield *
            }
            // MonadError
            override def errorClass: ErrorClass[e] = i
            override def throwError[a](l: e): m[a] = inner.`return`(Left(l))
            override def catchError[a](m: m[a])(h: e => m[a]): m[a] = {
                import inner.`for`
                for {
                    a <- m
                    * <- a match {
                        case Left(l) => h(l)
                        case Right(r) => inner.`return`(Right(r))
                    }
                } yield *
            }
            // Trans
            override def lift[a](n: n[a]): m[a] = {
                import inner.`for`
                for { a <- n } yield Right(a)
            }
        }
    }

    object ErrorT extends ErrorTLowPriorityImplicits {
        implicit def monadFix[e](implicit i: MonadFix[n], j: ErrorClass[e]): MonadFix[({type m[+a] = ErrorT[e, a]})#m] =
            new MonadFix[({type m[+a] = ErrorT[e, a]})#m] with MonadProxy[({type m[+a] = ErrorT[e, a]})#m]
        {
            private[this] type m[+a] = ErrorT[e, a]
            override val self = monad[e]
            override def mfix[a](f: (=> a) => m[a]): m[a] = {
                def k(a: => Either[e, a]) = f { a match {
                    case Right(r) => r
                    case _ => error("empty mfix argument")
                } }
                i.mfix(k)
            }
        }

        implicit def monadIO[e](implicit i: MonadIO[n], j: ErrorClass[e]): MonadIO[({type m[+a] = ErrorT[e, a]})#m] =
            new MonadIO[({type m[+a] = ErrorT[e, a]})#m] with MonadProxy[({type m[+a] = ErrorT[e, a]})#m]
        {
            private[this] type m[+a] = ErrorT[e, a]
            override val self = monad[e]
            override def liftIO[a](io: IO[a]): m[a] = self.lift(i.liftIO(io))
        }

        implicit def monadCont[e](implicit i: MonadCont[n], j: ErrorClass[e]): MonadCont[({type m[+a] = ErrorT[e, a]})#m] =
            new MonadCont[({type m[+a] = ErrorT[e, a]})#m] with MonadProxy[({type m[+a] = ErrorT[e, a]})#m]
        {
            private[this] type m[+a] = ErrorT[e, a]
            override val self = monad[e]
            override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = {
                i.callCC { (c: Either[e, a] => n[Either[e, b]]) =>
                    f(a => c(Right(a)))
                }
            }
        }

        implicit def monadReader[e, r](implicit i: MonadReader[r, n], j: ErrorClass[e]): MonadReader[r, ({type m[+a] = ErrorT[e, a]})#m] =
            new MonadReader[r, ({type m[+a] = ErrorT[e, a]})#m] with MonadProxy[({type m[+a] = ErrorT[e, a]})#m]
        {
            private[this] type m[+a] = ErrorT[e, a]
            override val self = monad[e]
            override def ask: m[r] = self.lift(i.ask)
            override def local[a](f: r => r)(m: m[a]): m[a] = i.local(f)(m)
        }

        implicit def monadWriter[e, w](implicit i: MonadWriter[w, n], j: ErrorClass[e]): MonadWriter[w, ({type m[+a] = ErrorT[e, a]})#m] =
            new MonadWriter[w, ({type m[+a] = ErrorT[e, a]})#m] with MonadProxy[({type m[+a] = ErrorT[e, a]})#m]
        {
            private[this] type m[+a] = ErrorT[e, a]
            override val self = monad[e]
            override def monoid: Monoid[w] = i.monoid
            override def tell(x: w): m[Unit] = self.lift(i.tell(x))
            override def listen[a](m: m[a]): m[(a, w)] = {
                import inner.`for`
                for { (a, w) <- i.listen(m) } yield (a match {
                    case Left(l) => Left(l)
                    case Right(r) => Right(r, w)
                })
            }
            override def pass[a](m: m[(a, w => w)]): m[a] = {
                import inner.`for`
                i.pass {
                    for {
                        a <- m
                    } yield (a match {
                        case Left(l) => (Left(l), id[w]_)
                        case Right((r, f)) => (Right(r), f)
                    })
                }
            }
        }
    }

// MaybeT
    sealed abstract class MaybeT[+a] {
        def run: n[Maybe[a]]
    }

    object MaybeT extends MonadPlus[MaybeT] with Trans[MaybeT] with InnerMonadMethod {
        def apply[a](_run: => n[Maybe[a]]): MaybeT[a] = new MaybeT[a] {
            override def run: n[Maybe[a]] = _run
        }

        def run[a](n: MaybeT[a]): n[Maybe[a]] = n.run

        // Monad
        private[this] type m[+a] = MaybeT[a]
        override def `return`[a](x: a): m[a] = MaybeT { inner.`return`(Just(x).up) }
        override def op_>>=[a, b](x: m[a])(y: a => m[b]): m[b] = MaybeT {
            run(x) >>= {
                case Nothing => inner.`return`(Nothing.of[b])
                case Just(v) => run(y(v))
            }
        }
        // MonadPlus
        override def mzero: m[Nothing] = MaybeT { inner.`return`(Nothing) }
        override def mplus[a](x: m[a])(y: => m[a]): m[a] = MaybeT {
            run(x) >>= {
                case Nothing => run(y)
                case Just(_) => run(x)
            }
        }
        // Trans
        override def lift[a](n: n[a]): m[a] = MaybeT { inner.liftM(Maybe.just[a])(n) }
    }

// StateT
    sealed abstract class StateT[s, +a] extends _StateT[s, n, a]

    trait StateTLowPriorityImplicits extends InnerMonadMethod { this: StateT.type =>
        implicit def monad[s]: MonadState[s, ({type m[+a] = StateT[s, a]})#m] with Trans[({type m[+a] = StateT[s, a]})#m] =
            new MonadState[s, ({type m[+a] = StateT[s, a]})#m] with Trans[({type m[+a] = StateT[s, a]})#m]
        {
            // Functor
            private[this] type f[+a] = StateT[s, a]
            override def fmap[a, b](f: a => b)(m: f[a]): f[b] = StateT { s =>
                for { (x, s_) <- run(m)(s) } yield (f(x), s_)
            }
            // Monad
            private[this] type m[+a] = f[a]
            override def `return`[a](a: a): m[a] = StateT { s => inner.`return`(a, s) }
            override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = StateT { s =>
                for { (a, s_) <- run(m)(s); * <- run(k(a))(s_) } yield *
            }
            // MonadState
            override def get: m[s] = StateT { s => inner.`return`(s, s) }
            override def put(s: s): m[Unit] = StateT { _ => inner.`return`((), s) }
            // Trans
            override def lift[a](n: n[a]): m[a] = StateT { s => for { a <- n } yield (a, s) }
        }
    }

    object StateT extends StateTLowPriorityImplicits {
        def apply[s, a](_run: s => n[(a, s)]): StateT[s, a] = new StateT[s, a] {
            override def run(s: s): n[(a, s)] = _run(s)
        }

        implicit def from[s, a](n: _StateT[s, n, a]): StateT[s, a] = StateT { n.run }

        def run[s, a](n: StateT[s, a]): s => n[(a, s)] = n.run

        def eval[s, a](n: StateT[s, a])(s: s): n[a] = for { (a, _) <- run(n)(s) } yield a

        def exec[s, a](n: StateT[s, a])(s: s): n[s] = for { (_, s) <- run(n)(s) } yield s

        def map[s, m[+_], a, b](f: n[(a, s)] => m[(b, s)])(n: StateT[s, a]): _StateT[s, m, b] = _StateT { f compose run(n) }

        def `with`[s, a](f: s => s)(n: StateT[s, a]): StateT[s, a] = StateT { run(n) compose f }

        implicit def monadPlus[s](implicit i: MonadPlus[n]): MonadPlus[({type m[+a] = StateT[s, a]})#m] =
            new MonadPlus[({type m[+a] = StateT[s, a]})#m] with MonadProxy[({type m[+a] = StateT[s, a]})#m]
        {
            private[this] type m[+a] = StateT[s, a]
            override val self = monad[s]
            override def mzero: m[Nothing] = StateT { _ => i.mzero }
            override def mplus[a](m: m[a])(n: => m[a]): m[a] = StateT { s => i.mplus(run(m)(s))(run(n)(s)) }
        }

        implicit def monadFix[s](implicit i: MonadFix[n]): MonadFix[({type m[+a] = StateT[s, a]})#m] =
            new MonadFix[({type m[+a] = StateT[s, a]})#m] with MonadProxy[({type m[+a] = StateT[s, a]})#m]
        {
            private[this] type m[+a] = StateT[s, a]
            override val self = monad[s]
            override def mfix[a](f: (=> a) => m[a]): m[a] = StateT { s =>
                def k(aI_ : => (a, s)) = run(f(aI_._1))(s)
                i.mfix(k)
                // scalac sucks.
                // i.mfix { (aI_ : (=> (a, s))) => run(f(aI_._1))(s) }
            }
        }

        implicit def monadIO[s](implicit i: MonadIO[n]): MonadIO[({type m[+a] = StateT[s, a]})#m] =
            new MonadIO[({type m[+a] = StateT[s, a]})#m] with MonadProxy[({type m[+a] = StateT[s, a]})#m]
        {
            private[this] type m[+a] = StateT[s, a]
            override val self = monad[s]
            override def liftIO[a](io: IO[a]): m[a] = self.lift(i.liftIO(io))
        }

        implicit def monadCont[s](implicit i: MonadCont[n]): MonadCont[({type m[+a] = StateT[s, a]})#m] =
            new MonadCont[({type m[+a] = StateT[s, a]})#m] with MonadProxy[({type m[+a] = StateT[s, a]})#m]
        {
            private[this] type m[+a] = StateT[s, a]
            override val self = monad[s]
            override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = StateT { s =>
                i.callCC { (c: ((a, s)) => n[(b, s)]) =>
                    run( f(a => StateT { s_ => c((a, s_)) }) )(s)
                }
            }
        }

        implicit def monadError[s, e](implicit i: MonadError[e, n]): MonadError[e, ({type m[+a] = StateT[s, a]})#m] =
            new MonadError[e, ({type m[+a] = StateT[s, a]})#m] with MonadProxy[({type m[+a] = StateT[s, a]})#m]
        {
            private[this] type m[+a] = StateT[s, a]
            override val self = monad[s]
            override def errorClass: ErrorClass[e] = i.errorClass
            override def throwError[a](e: e): m[a] = self.lift(i.throwError(e))
            override def catchError[a](m: m[a])(h: e => m[a]): m[a] = StateT { s =>
                i.catchError(run(m)(s)) { e =>
                    run(h(e))(s)
                }
            }
        }

        implicit def monadReader[s, r](implicit i: MonadReader[r, n]): MonadReader[r, ({type m[+a] = StateT[s, a]})#m] =
            new MonadReader[r, ({type m[+a] = StateT[s, a]})#m] with MonadProxy[({type m[+a] = StateT[s, a]})#m]
        {
            private[this] type m[+a] = StateT[s, a]
            override val self = monad[s]
            override def ask: m[r] = self.lift(i.ask)
            override def local[a](f: r => r)(m: m[a]): m[a] = StateT { s => i.local(f)(run(m)(s)) }
        }

        implicit def monadWriter[s, w](implicit i: MonadWriter[w, n]): MonadWriter[w, ({type m[+a] = StateT[s, a]})#m] =
            new MonadWriter[w, ({type m[+a] = StateT[s, a]})#m] with MonadProxy[({type m[+a] = StateT[s, a]})#m]
        {
            private[this] type m[+a] = StateT[s, a]
            override val self = monad[s]
            override def monoid: Monoid[w] = i.monoid
            override def tell(x: w): m[Unit] = self.lift(i.tell(x))
            override def listen[a](m: m[a]): m[(a, w)] = StateT { s =>
                for { ((a, s_), w) <- i.listen(run(m)(s)) } yield ((a, w), s_)
            }
            override def pass[a](m: m[(a, w => w)]): m[a] = StateT { s =>
                i.pass {
                    for { ((a, f), s_) <- run(m)(s) } yield ((a, s_), f)
                }
            }
        }
    }

// ReaderT
    sealed abstract class ReaderT[r, +a] extends _ReaderT[r, n, a]

    trait ReaderTLowPriorityImplicits extends InnerMonadMethod { this: ReaderT.type =>
        implicit def monad[r]: MonadReader[r, ({type m[+a] = ReaderT[r, a]})#m] with Trans[({type m[+a] = ReaderT[r, a]})#m] =
            new MonadReader[r, ({type m[+a] = ReaderT[r, a]})#m] with Trans[({type m[+a] = ReaderT[r, a]})#m]
        {
            // Functor
            private[this] type f[+a] = ReaderT[r, a]
            override def fmap[a, b](f: a => b)(m: f[a]): f[b] = ReaderT { r =>
                for { a <- run(m)(r) } yield f(a)
            }
            // Monad
            private[this] type m[+a] = f[a]
            override def `return`[a](a: a): m[a] = ReaderT { r => inner.`return`(a) }
            override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = ReaderT { r =>
                for { a <- run(m)(r); * <- run(k(a))(r) } yield *
            }
            // MonadReader
            override def ask: m[r] = ReaderT { inner.`return` }
            override def local[a](f: r => r)(m: m[a]): m[a] = ReaderT { r => run(m)(f(r)) }
            // Trans
            override def lift[a](n: n[a]): m[a] = ReaderT { _ => n }
        }
    }

    object ReaderT extends ReaderTLowPriorityImplicits {
        def apply[r, a](_run: r => n[a]): ReaderT[r, a] = new ReaderT[r, a] {
            override def run(r: r): n[a] = _run(r)
        }

        implicit def from[r, a](n: _ReaderT[r, n, a]): ReaderT[r, a] = ReaderT { n.run }

        def run[r, a](n: ReaderT[r, a]): r => n[a] = n.run

        def map[r, m[+_], a, b](f: n[a] => m[b])(n: ReaderT[r, a]): _ReaderT[r, m, b] = _ReaderT { f compose run(n) }

        def `with`[r, r_, a](f: r_ => r)(n: ReaderT[r, a]): ReaderT[r_, a] = ReaderT { run(n) compose f }

        implicit def monadPlus[r](implicit i: MonadPlus[n]): MonadPlus[({type m[+a] = ReaderT[r, a]})#m] =
            new MonadPlus[({type m[+a] = ReaderT[r, a]})#m] with MonadProxy[({type m[+a] = ReaderT[r, a]})#m]
        {
            private[this] type m[+a] = ReaderT[r, a]
            override val self = monad[r]
            override def mzero: m[Nothing] = ReaderT { _ => i.mzero }
            override def mplus[a](m: m[a])(n: => m[a]): m[a] = ReaderT { r => i.mplus(run(m)(r))(run(n)(r)) }
        }

        implicit def monadFix[r](implicit i: MonadFix[n]): MonadFix[({type m[+a] = ReaderT[r, a]})#m] =
            new MonadFix[({type m[+a] = ReaderT[r, a]})#m] with MonadProxy[({type m[+a] = ReaderT[r, a]})#m]
        {
            private[this] type m[+a] = ReaderT[r, a]
            override val self = monad[r]
            override def mfix[a](f: (=> a) => m[a]): m[a] = ReaderT { r =>
                def k(a: => a) = run(f(a))(r)
                i.mfix(k)
            }
        }

        implicit def monadIO[r](implicit i: MonadIO[n]): MonadIO[({type m[+a] = ReaderT[r, a]})#m] =
            new MonadIO[({type m[+a] = ReaderT[r, a]})#m] with MonadProxy[({type m[+a] = ReaderT[r, a]})#m]
        {
            private[this] type m[+a] = ReaderT[r, a]
            override val self = monad[r]
            override def liftIO[a](io: IO[a]): m[a] = self.lift(i.liftIO(io))
        }

        implicit def monadCont[r](implicit i: MonadCont[n]): MonadCont[({type m[+a] = ReaderT[r, a]})#m] =
            new MonadCont[({type m[+a] = ReaderT[r, a]})#m] with MonadProxy[({type m[+a] = ReaderT[r, a]})#m]
        {
            private[this] type m[+a] = ReaderT[r, a]
            override val self = monad[r]
            override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = ReaderT { r =>
                i.callCC { (c: a => n[b]) =>
                    run( f(a => ReaderT { s_ => c(a) }) )(r)
                }
            }
        }

        implicit def monadError[r, e](implicit i: MonadError[e, n]): MonadError[e, ({type m[+a] = ReaderT[r, a]})#m] =
            new MonadError[e, ({type m[+a] = ReaderT[r, a]})#m] with MonadProxy[({type m[+a] = ReaderT[r, a]})#m]
        {
            private[this] type m[+a] = ReaderT[r, a]
            override val self = monad[r]
            override def errorClass: ErrorClass[e] = i.errorClass
            override def throwError[a](e: e): m[a] = self.lift(i.throwError(e))
            override def catchError[a](m: m[a])(h: e => m[a]): m[a] = ReaderT { r =>
                i.catchError(run(m)(r)) { e =>
                    run(h(e))(r)
                }
            }
        }

        implicit def monadState[r, s](implicit i: MonadState[s, n]): MonadState[s, ({type m[+a] = ReaderT[r, a]})#m] =
            new MonadState[s, ({type m[+a] = ReaderT[r, a]})#m] with MonadProxy[({type m[+a] = ReaderT[r, a]})#m]
        {
            private[this] type m[+a] = ReaderT[r, a]
            override val self = monad[r]
            override def get: m[s] = self.lift(i.get)
            override def put(s: s): m[Unit] = self.lift(i.put(s))
        }

        implicit def monadWriter[r, w](implicit i: MonadWriter[w, n]): MonadWriter[w, ({type m[+a] = ReaderT[r, a]})#m] =
            new MonadWriter[w, ({type m[+a] = ReaderT[r, a]})#m] with MonadProxy[({type m[+a] = ReaderT[r, a]})#m]
        {
            private[this] type m[+a] = ReaderT[r, a]
            override val self = monad[r]
            override def monoid: Monoid[w] = i.monoid
            override def tell(x: w): m[Unit] = self.lift(i.tell(x))
            override def listen[a](m: m[a]): m[(a, w)] = ReaderT { w => i.listen(run(m)(w)) }
            override def pass[a](m: m[(a, w => w)]): m[a] = ReaderT { w => i.pass(run(m)(w)) }
        }
    }

// WriterT
    sealed abstract class WriterT[w, +a] extends _WriterT[w, n, a]

    trait WriterTLowPriorityImplicits extends InnerMonadMethod { this: WriterT.type =>
        implicit def monad[w](implicit i: Monoid[w]): MonadWriter[w, ({type m[+a] = WriterT[w, a]})#m] with Trans[({type m[+a] = WriterT[w, a]})#m] =
            new MonadWriter[w, ({type m[+a] = WriterT[w, a]})#m] with Trans[({type m[+a] = WriterT[w, a]})#m]
        {
            // Functor
            private[this] type f[+a] = WriterT[w, a]
            override def fmap[a, b](f: a => b)(m: f[a]): f[b] = WriterT {
                for { (a, w) <- run(m) } yield (f(a), w)
            }
            // Monad
            private[this] type m[+a] = f[a]
            override def `return`[a](a: a): m[a] = WriterT { inner.`return`(a, i.mempty) }
            override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = WriterT {
                for { (a, w) <- run(m); (b, w_) <- run(k(a)) } yield (b, i.mappend(w)(w_))
            }
            // MonadWriter
            override def monoid: Monoid[w] = i
            override def tell(w: w): m[Unit] = WriterT { inner.`return`((), w) }
            override def listen[a](m: m[a]): m[(a, w)] = WriterT {
                for { (a, w) <- run(m) } yield ((a, w), w)
            }
            override def pass[a](m: m[(a, w => w)]): m[a] = WriterT {
                for { ((a, f), w) <- run(m) } yield (a, f(w))
            }
            // Trans
            override def lift[a](n: n[a]): m[a] = WriterT {
                for { a <- n } yield (a, i.mempty)
            }
        }
    }

    object WriterT extends WriterTLowPriorityImplicits {
        def apply[w, a](_run: => n[(a, w)])(implicit i: Monoid[w]): WriterT[w, a] = new WriterT[w, a] {
            override def monoid: Monoid[w] = i
            override def run: n[(a, w)] = _run
        }

        implicit def from[w, a](n: _WriterT[w, n, a]): WriterT[w, a] = WriterT(n.run)(n.monoid)

        def run[w, a](n: WriterT[w, a]): n[(a, w)] = n.run

        def exec[w, a](n: WriterT[w, a]): n[w] = for { (_, w) <- run(n) } yield w

        def map[w, w_, m[+_], a, b](f: n[(a, w)] => m[(b, w_)])(n: WriterT[w, a])(implicit i: Monoid[w_]): _WriterT[w_, m, b] = _WriterT { f(run(n)) }

        implicit def monadPlus[w](implicit i: MonadPlus[n], j: Monoid[w]): MonadPlus[({type m[+a] = WriterT[w, a]})#m] =
            new MonadPlus[({type m[+a] = WriterT[w, a]})#m] with MonadProxy[({type m[+a] = WriterT[w, a]})#m]
        {
            private[this] type m[+a] = WriterT[w, a]
            override val self = monad[w]
            override def mzero: m[Nothing] = WriterT { i.mzero }
            override def mplus[a](m: m[a])(n: => m[a]): m[a] = WriterT { i.mplus(run(m))(run(n)) }
        }

        implicit def monadFix[w](implicit i: MonadFix[n], j: Monoid[w]): MonadFix[({type m[+a] = WriterT[w, a]})#m] =
            new MonadFix[({type m[+a] = WriterT[w, a]})#m] with MonadProxy[({type m[+a] = WriterT[w, a]})#m]
        {
            private[this] type m[+a] = WriterT[w, a]
            override val self = monad[w]
            override def mfix[a](m: (=> a) => m[a]): m[a] = WriterT {
                def k(aI_ : => (a, w)) = run(m(aI_._1))
                i.mfix(k)
            }
        }

        implicit def monadIO[w](implicit i: MonadIO[n], j: Monoid[w]): MonadIO[({type m[+a] = WriterT[w, a]})#m] =
            new MonadIO[({type m[+a] = WriterT[w, a]})#m] with MonadProxy[({type m[+a] = WriterT[w, a]})#m]
        {
            private[this] type m[+a] = WriterT[w, a]
            override val self = monad[w]
            override def liftIO[a](io: IO[a]): m[a] = self.lift(i.liftIO(io))
        }

        implicit def monadCont[w](implicit i: MonadCont[n], j: Monoid[w]): MonadCont[({type m[+a] = WriterT[w, a]})#m] =
            new MonadCont[({type m[+a] = WriterT[w, a]})#m] with MonadProxy[({type m[+a] = WriterT[w, a]})#m]
        {
            private[this] type m[+a] = WriterT[w, a]
            override val self = monad[w]
            override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = WriterT {
                i.callCC { (c: ((a, w)) => n[(b, w)]) =>
                    run( f(a => WriterT { c((a, j.mempty)) }) )
                }
            }
        }

        implicit def monadError[w, e](implicit i: MonadError[e, n], j: Monoid[w]): MonadError[e, ({type m[+a] = WriterT[w, a]})#m] =
            new MonadError[e, ({type m[+a] = WriterT[w, a]})#m] with MonadProxy[({type m[+a] = WriterT[w, a]})#m]
        {
            private[this] type m[+a] = WriterT[w, a]
            override val self = monad[w]
            override def errorClass: ErrorClass[e] = i.errorClass
            override def throwError[a](e: e): m[a] = self.lift(i.throwError(e))
            override def catchError[a](m: m[a])(h: e => m[a]): m[a] = WriterT {
                i.catchError(run(m)) { e =>
                    run(h(e))
                }
            }
        }

        implicit def monadReader[w, r](implicit i: MonadReader[r, n], j: Monoid[w]): MonadReader[r, ({type m[+a] = WriterT[w, a]})#m] =
            new MonadReader[r, ({type m[+a] = WriterT[w, a]})#m] with MonadProxy[({type m[+a] = WriterT[w, a]})#m]
        {
            private[this] type m[+a] = WriterT[w, a]
            override val self = monad[w]
            override def ask: m[r] = self.lift(i.ask)
            override def local[a](f: r => r)(m: m[a]): m[a] = WriterT { i.local(f)(run(m)) }
        }

        implicit def monadState[w, s](implicit i: MonadState[s, n], j: Monoid[w]): MonadState[s, ({type m[+a] = WriterT[w, a]})#m] =
            new MonadState[s, ({type m[+a] = WriterT[w, a]})#m] with MonadProxy[({type m[+a] = WriterT[w, a]})#m]
        {
            private[this] type m[+a] = WriterT[w, a]
            override val self = monad[w]
            override def get: m[s] = self.lift(i.get)
            override def put(s: s): m[Unit] = self.lift(i.put(s))
        }
    }
}


/**
 * Types for conversation between MonadTs
 */
object MonadT {

// StateT
    trait _StateT[s, m[+_], +a] extends Up[_StateT[s, m, a]] {
        def run(s: s): m[(a, s)]
    }

    trait _StateTProxy[s, m[+_], +a] extends _StateT[s, m, a] with Proxy {
        override def self: _StateT[s, m, a]
        override def run(s: s): m[(a, s)] = self.run(s)
    }

    object _StateT {
        def apply[s, m[+_], a](_run: s => m[(a, s)]): _StateT[s, m, a] = new _StateT[s, m, a] {
            override def run(s: s): m[(a, s)] = _run(s)
        }
    }

// ReaderT
    trait _ReaderT[r, m[+_], +a] extends Up[_ReaderT[r, m, a]] {
        def run(r: r): m[a]
    }

    trait _ReaderTProxy[r, m[+_], +a] extends _ReaderT[r, m, a] with Proxy {
        override def self: _ReaderT[r, m, a]
        override def run(r: r): m[a] = self.run(r)
    }

    object _ReaderT {
        def apply[r, m[+_], a](_run: r => m[a]): _ReaderT[r, m, a] = new _ReaderT[r, m, a] {
            override def run(r: r): m[a] = _run(r)
        }
    }

// WriterT
    sealed abstract class _WriterT[w, m[+_], +a] extends Up[_WriterT[w, m, a]] {
        def monoid: Monoid[w]
        def run: m[(a, w)]
    }

    trait _WriterTProxy[w, m[+_], +a] extends _WriterT[w, m, a] with Proxy {
        override def self: _WriterT[w, m, a]
        override def monoid: Monoid[w] = self.monoid
        override def run: m[(a, w)] = self.run
    }

    object _WriterT {
        def apply[w, m[+_], a](_run: => m[(a, w)])(implicit i: Monoid[w]): _WriterT[w, m, a] = new _WriterT[w, m, a] {
            override def monoid: Monoid[w] = i
            override def run: m[(a, w)] = _run
        }
    }
}
