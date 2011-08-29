

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2010 John Millikin
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package enumerator


private[ken] trait Types[n[+_]] { this: _Enumerators[n] =>
    val inner: Monad[n]

    // Stream
    //
    sealed abstract class Stream[+a] extends Up[Stream[a]]
    final case class Chunks[+a](_1: List[a]) extends Stream[a]
    final case object EOF extends Stream[Nothing]

    object Stream extends Monad[Stream] with ThisIsInstance {
        // Overrides
        //
        // Monad
        private[this] type m[+a] = Stream[a]
        override def `return`[a](a: Lazy[a]): m[a] = Chunks(List.`return`(a))
        override def op_>>=[a, b](m: m[a])(f: a => m[b]): m[b] = m match {
            case Chunks(xs) => _asMonoid[b].mconcat(List.fmap(f)(xs))
            case EOF => EOF
        }

        // Instances
        //
        implicit def _asMonoid[a]: Monoid[Stream[a]] = new Monoid[Stream[a]] {
            private[this] type m = Stream[a]
            val i = Monoid[List[a]]
            override def mempty: m = Chunks(i.mempty)
            override def mappend: m => Lazy[m] => m = x => y => (x, y.!) match {
                case (Chunks(xs), Chunks(ys)) => Chunks(i.mappend(xs)(ys))
                case _ => EOF
            }
        }
    }

    // Step
    //
    sealed abstract class Step[-a, +b]
    final case class Continue[a, b](_1: Stream[a] => Iteratee[a, b]) extends Step[a, b]
    final case class Yield[a, b](_1: b, _2: Stream[a]) extends Step[a, b]
    final case class Error(_1: Throwable) extends Step[Any, Nothing]

    // Iteratee
    //
    final case class Iteratee[-a, +b](override val get: n[Step[a, b]]) extends NewtypeOf[n[Step[a, b]]] with Kind.constThis {
        def >>==[a_, b_](f: Step[a, b] => Iteratee[a_, b_]): Iteratee[a_, b_] = op_>>==(this)(f)
    }

    object Iteratee extends Iteratee_ with Kind.FunctionLike {
        sealed trait apply[z] extends Kind.AbstractMonadTrans {
            override type apply1[+a] = Iteratee[z, a]
            override type oldtype1[+a] = n[Step[z, a]]
            override type innerMonad[+a] = n[a]
        }

        implicit def dependent[a, b](n: NewtypeOf[n[Step[a, b]]]): Iteratee[a, b] = Iteratee { n.run }

        def run[a, b](n: Iteratee[a, b]): n[Step[a, b]] = n.run

        def map[m[+_], a, a_, b, b_](f: n[Step[a, b]] => m[Step[a_, b_]])(n: Iteratee[a, b]): NewtypeOf[m[Step[a_, b_]]] = NewtypeOf { f(run(n)) }
    }

    def runIteratee[a, b](n: Iteratee[a, b]): n[Step[a, b]] = n.run

    def returnI[a, b](step: Step[a, b]): Iteratee[a, b] = Iteratee { inner.`return`(step) }
    def `yield`[a, b](x: b)(extra: Stream[a]): Iteratee[a, b] = returnI(Yield(x, extra))
    def continue[a, b](k: Stream[a] => Iteratee[a, b]): Iteratee[a, b] = returnI(Continue(k))

    // Enumerator
    //
    type Enumerator[a, b] = Step[a, b] => Iteratee[a, b]

    // Enumeratee
    //
    type Enumeratee[ao, ai, b] = Step[ai, b] => Iteratee[ao, Step[ai, b]]

    private[ken] trait Iteratee_0 { this: Iteratee.type =>
        implicit def _asNewtype1[z]: Newtype1[({type nt[+a] = Iteratee[z, a]})#nt, ({type ot[+a] = n[Step[z, a]]})#ot] = new Newtype1[({type nt[+a] = Iteratee[z, a]})#nt, ({type ot[+a] = n[Step[z, a]]})#ot] {
            private[this] type nt[+a] = Iteratee[z, a]
            private[this] type ot[+a] = n[Step[z, a]]
            override def newOf[a](ot: Lazy[ot[a]]): nt[a] = Iteratee(ot)
            override def oldOf[a](nt: Lazy[nt[a]]): ot[a] = nt.run
        }

        implicit def _asMonad[z]: Monad[({type m[+a] = Iteratee[z, a]})#m] = new Monad[({type m[+a] = Iteratee[z, a]})#m] {
            private[this] type m[+a] = Iteratee[z, a]
            override def `return`[a](x: Lazy[a]): m[a] = `yield`(x.!)(Chunks(Nil.of[a]).up)
            override def op_>>=[a, b](m0: m[a])(f: a => m[b]): m[b] = Function.fix {
                (bind: Lazy[m[a] => m[b]]) => (m: m[a]) => Iteratee {
                    import inner.>>=
                    runIteratee(m) >>= {
                        case Continue(k) => inner.`return`(Continue(bind compose k))
                        case Error(err) => inner.`return`(Error(err))
                        case Yield(x, Chunks(Nil)) => runIteratee(f(x))
                        case Yield(x, extra) => runIteratee(f(x)) >>= {
                            case Continue(k) => runIteratee(k(extra))
                            case Error(err) => inner.`return`(Error(err))
                            case Yield(x_, _) => inner.`return`(Yield(x_, extra))
                        }
                    }
                }
            }.apply(m0)
        }

        implicit def _asMonadTrans[z]: MonadTrans[n, ({type m[+a] = Iteratee[z, a]})#m] = new MonadTrans[n, ({type m[+a] = Iteratee[z, a]})#m] {
            private[this] type m[+a] = Iteratee[z, a]
            override def lift[a](n: n[a]): m[a] = Iteratee {
                import inner.>>=
                n >>= { runIteratee[z, a]_ compose _asMonad[z].`return`[a] }
            }
        }
    }

    private[ken] trait Iteratee_ extends Iteratee_0 { this: Iteratee.type =>
        implicit def _asMonadIO[z](implicit i: MonadIO[n]): MonadIO[({type m[+a] = Iteratee[z, a]})#m] = new MonadIO[({type m[+a] = Iteratee[z, a]})#m] with MonadProxy[({type m[+a] = Iteratee[z, a]})#m] {
            private[this] type m[+a] = Iteratee[z, a]
            override val selfMonad = _asMonad[z]
            override def liftIO[a](io: IO[a]): m[a] = _asMonadTrans.lift(i.liftIO(io))
        }
    }
}
