

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

    trait apply2[z, n[+_]] extends Kind.Newtype1 {
        override type apply1[+a] = Iteratee[z, n, a]
        override type oldtype1[+a] = n[Step[z, n, a]]
    }
}

private[enumerator] sealed trait IterateeAs0 { this: Iteratee.type =>
/*
    implicit def _asNewtype1[z]: Newtype1[({type nt[+a] = Iteratee[z, a]})#nt, ({type ot[+a] = n[Step[z, a]]})#ot] = new Newtype1[({type nt[+a] = Iteratee[z, a]})#nt, ({type ot[+a] = n[Step[z, a]]})#ot] {
        private type nt[+a] = Iteratee[z, a]
        private type ot[+a] = n[Step[z, a]]
        override def newOf[a](ot: Lazy[ot[a]]): nt[a] = Iteratee(ot)
        override def oldOf[a](nt: Lazy[nt[a]]): ot[a] = nt.run
    }
*/
    import Enumerator.runIteratee

    implicit def _asMonad[z, n[+_]](implicit i: Monad[n]): Monad[apply2[z, n]#apply1] = new Monad[apply2[z, n]#apply1] {
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

    implicit def _asMonadTrans[z]: MonadTrans[apply1[z]#monadTrans] = new MonadTrans[apply1[z]#monadTrans] {
        private type t[n[+_], +a] = Iteratee[z, n, a]
        override def lift[n[+_], a](n: n[a])(implicit i: Monad[n]): t[n, a] = Iteratee {
            import i.>>=
            n >>= { Enumerator.runIteratee[z, n, a]_ `.` _asMonad[z, n].`return`[a] }
        }
    }
}

private[enumerator] trait IterateeAs extends IterateeAs0 { this: Iteratee.type =>
    implicit def _asMonadIO[z, n[+_]](implicit i: MonadIO[n]): MonadIO[apply2[z, n]#apply1] = new MonadIO[apply2[z, n]#apply1] with MonadProxy[apply2[z, n]#apply1] {
        private type m[+a] = Iteratee[z, n, a]
        override val selfMonad = _asMonad[z, n]
        override def liftIO[a](io: IO[a]): m[a] = _asMonadTrans[z].lift(i.liftIO(io))
    }
}
