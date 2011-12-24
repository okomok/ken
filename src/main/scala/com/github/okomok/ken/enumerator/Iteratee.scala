

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2010 John Millikin
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package enumerator


final case class Iteratee[-a, +n[+_], +b](override val old: n[Step[a, n, b]]) extends NewtypeOf[n[Step[a, n, b]]]


object Iteratee extends IterateeAs with Kind.FunctionLike {
    trait apply[z] extends apply1[z]
    trait apply1[z] extends Kind.MonadTrans {
        override type monadTrans[n[+_], +a] = Iteratee[z, n, a]
    }

    trait apply2[z, n <: Kind.Function1] extends Kind.Newtype1 {
        override type apply1[+a] = Iteratee[z, n#apply1, a]
        override type oldtype1[+a] = n#apply1[Step[z, n#apply1, a]]
    }
}

private[enumerator] sealed trait IterateeAs0 { this: Iteratee.type =>
    import Enumerator.runIteratee

    implicit def _asMonad[z, n[+_]](implicit i: Monad[n]): Monad[({type L[+a] = Iteratee[z, n, a]})#L] = new Monad[({type L[+a] = Iteratee[z, n, a]})#L] {
        private type m[+a] = Iteratee[z, n, a]
        override def `return`[a](x: Lazy[a]): m[a] = Enumerator.`yield`(x.!)(Chunks(Nil.of[z]).up)(i)
        override def op_>>=[a, b](m0: m[a])(f: a => m[b]): m[b] = Function.fix {
            (bind: Lazy[m[a] => m[b]]) => (m: m[a]) => Iteratee {
                import i.>>=
                Enumerator.runIteratee(m) >>= {
                    case Continue(k) => i.`return`(Continue(bind.! `.` k))
                    case Error(err) => i.`return`(Error(err))
                    case Yield(x, Chunks(Nil)) => runIteratee(f(x))
                    case Yield(x, extra) => runIteratee(f(x)) >>= {
                        case Continue(k) => runIteratee(k(extra.asInstanceOf[Stream[z]]))
                        case Error(err) => i.`return`(Error(err))
                        case Yield(x_, _) => i.`return`(Yield(x_, extra))
                    }
                }
            }
        }.apply(m0)
    }

    implicit def _asMonadTrans[z]: MonadTrans[({type L[n[+_], +a] = Iteratee[z, n, a]})#L] = new MonadTrans[({type L[n[+_], +a] = Iteratee[z, n, a]})#L] {
        private type t[n[+_], +a] = Iteratee[z, n, a]
        override def lift[n[+_], a](n: n[a])(implicit i: Monad[n]): t[n, a] = Iteratee {
            import i.>>=
            n >>= { Enumerator.runIteratee[z, n, a]_ `.` _asMonad[z, n].`return`[a] }
        }
    }
}

private[enumerator] trait IterateeAs extends IterateeAs0 { this: Iteratee.type =>
    implicit def _asMonadIO[z, n[+_]](implicit i: MonadIO[n]): MonadIO[({type L[+a] = Iteratee[z, n, a]})#L] = new MonadIO[({type L[+a] = Iteratee[z, n, a]})#L] with MonadProxy[({type L[+a] = Iteratee[z, n, a]})#L] {
        private type m[+a] = Iteratee[z, n, a]
        override val selfMonad: selfMonad = _asMonad[z, n]
        override def liftIO[a](io: IO[a]): m[a] = _asMonadTrans[z].lift(i.liftIO(io))
    }
}
