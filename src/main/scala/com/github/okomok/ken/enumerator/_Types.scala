

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2010 John Millikin
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package enumerator


private[enumerator] trait _Types[n[+_]] { this: _Enumerators[n] =>

    // Step
    //
    sealed trait Step[-a, +b] extends Step_[a, n, b]
    final case class Continue[a, b](override val k: Stream[a] => Iteratee[a, b]) extends Continue_[a, n, b] with Step[a, b]
    final case class Yield[a, b](override val x: b, override val extra: Stream[a]) extends Yield_[a, n, b] with Step[a, b]
    final case class Error(override val err: SomeException) extends Error_[n] with Step[Any, Nothing]

    object Step {
        def dependent[a, b](s: Step_[a, n, b]): Step[a, b] = {
            if (s.isInstanceOf[Continue_Tag]) {
                val s_ = s.asInstanceOf[Continue_[a, n, b]]
                Continue(in => Iteratee.dependent(s_.k(in)))
            } else if (s.isInstanceOf[Yield_Tag]) {
                val s_ = s.asInstanceOf[Yield_[a, n, b]]
                Yield(s_.x, s_.extra)
            } else if (s.isInstanceOf[Error_Tag]) {
                val s_ = s.asInstanceOf[Error_[n]]
                Error(s_.err)
            } else {
                error("impossible")
            }
        }
    }

    // Iteratee (reaction)
    //
    final case class Iteratee[-a, +b](override val get: n[Step[a, b]]) extends NewtypeOf[n[Step[a, b]]]

    object Iteratee extends Iteratee_ with Kind.FunctionLike {
        sealed trait apply[z] extends Kind.MonadTrans {
            override type apply1[+a] = Iteratee[z, a]
            override type oldtype1[+a] = n[Step[z, a]]
            override type innerMonad[+a] = n[a]
        }

        implicit def dependent[a, b](n: NewtypeOf[n[Step_[a, n, b]]]): Iteratee[a, b] = Iteratee {
            for {
                s <- n.run
            } yield Step.dependent(s)
        }

        // unneeded if a bug is fixed.
        implicit def dependentEnumerator[a, b](e: Step_[a, n, b] => NewtypeOf[n[Step_[a, n, b]]]): Step[a, b] => Iteratee[a, b] = e
        implicit def dependentEnumeratee[ao, ai, b](e: Step_[ai, n, b] => NewtypeOf[n[Step_[ao, n, Step[ai, b]]]]): Step[ai, b] => Iteratee[ao, Step[ai, b]] = e

        def run[a, b](n: Iteratee[a, b]): n[Step[a, b]] = n.run

        def map[m[+_], a, a_, b, b_](f: n[Step[a, b]] => m[Step[a_, b_]])(n: Iteratee[a, b]): NewtypeOf[m[Step[a_, b_]]] = NewtypeOf { f(run(n)) }
    }

    def runIteratee[a, b](n: Iteratee[a, b]): n[Step[a, b]] = n.run

    def returnI[a, b](s: Step[a, b]): Iteratee[a, b] = Iteratee { inner.`return`(s) }
    def `yield`[a, b](x: b)(extra: Stream[a]): Iteratee[a, b] = returnI(Yield(x, extra))
    def continue[a, b](k: Stream[a] => Iteratee[a, b]): Iteratee[a, b] = returnI(Continue(k))

    // Enumerator (seqeuence)
    //
    type Enumerator[a, b] = Step[a, b] => Iteratee[a, b]

    // Enumeratee
    //
    type Enumeratee[ao, ai, b] = Step[ai, b] => Iteratee[ao, Step[ai, b]]

    // Instances
    //
    private[enumerator] trait Iteratee_0 { this: Iteratee.type =>
        implicit def _asNewtype1[z]: Newtype1[({type nt[+a] = Iteratee[z, a]})#nt, ({type ot[+a] = n[Step[z, a]]})#ot] = new Newtype1[({type nt[+a] = Iteratee[z, a]})#nt, ({type ot[+a] = n[Step[z, a]]})#ot] {
            private type nt[+a] = Iteratee[z, a]
            private type ot[+a] = n[Step[z, a]]
            override def newOf[a](ot: Lazy[ot[a]]): nt[a] = Iteratee(ot)
            override def oldOf[a](nt: Lazy[nt[a]]): ot[a] = nt.run
        }

        implicit def _asMonad[z]: Monad[({type m[+a] = Iteratee[z, a]})#m] = new Monad[({type m[+a] = Iteratee[z, a]})#m] {
            private type m[+a] = Iteratee[z, a]
            override def `return`[a](x: Lazy[a]): m[a] = `yield`(x.!)(Chunks(Nil.of[a]).up)
            override def op_>>=[a, b](m0: m[a])(f: a => m[b]): m[b] = Function.fix {
                (bind: Lazy[m[a] => m[b]]) => (m: m[a]) => Iteratee {
                    runIteratee(m) >>= {
                        case Continue(k) => inner.`return`(Continue(bind.! `.` k))
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
            private type m[+a] = Iteratee[z, a]
            override def lift[a](n: n[a]): m[a] = Iteratee {
                n >>= { runIteratee[z, a]_ `.` _asMonad[z].`return`[a] }
            }
        }
    }

    private[enumerator] trait Iteratee_ extends Iteratee_0 { this: Iteratee.type =>
        implicit def _asMonadIO[z](implicit i: MonadIO[n]): MonadIO[({type m[+a] = Iteratee[z, a]})#m] = new MonadIO[({type m[+a] = Iteratee[z, a]})#m] with MonadProxy[({type m[+a] = Iteratee[z, a]})#m] {
            private type m[+a] = Iteratee[z, a]
            override val selfMonad = _asMonad[z]
            override def liftIO[a](io: IO[a]): m[a] = _asMonadTrans.lift(i.liftIO(io))
        }
    }
}
