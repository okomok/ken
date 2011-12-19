

// Public domain


package com.github.okomok.kentest.example


import com.github.okomok.ken._


// http://comonad.com/reader/2011/free-monads-for-less/


class FreeMonadsForLess extends org.scalatest.junit.JUnit3Suite {


    sealed abstract class Free[+f[+_], +a] {
        final def up: Free[f, a] = this
    }
    final case class Pure[a](_1: a) extends Free[Nothing, a]
    final case class Roll[f[+_], a](_1: f[Free[f, a]]) extends Free[f, a]

    object Free extends MonadTrans[Free] {
        trait apply1[f[+_]] extends Kind.Function1 {
            override type apply1[+a] = Free[f, a]
        }

        implicit def _asMonad[f[+_]](implicit i: Functor[f]): Monad[apply1[f]#apply1] = new Monad[({type m[+a] = Free[f, a]})#m] {
            private type m[+a] = Free[f, a]
            override def `return`[a](x: Lazy[a]): m[a] = Pure(x)
            override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = m match {
                case Pure(a) => k(a)
                case Roll(m) => Roll { i.fmap((f: Free[f, a]) => f >>= k)(m) }
            }
        }

        // as MonadTrans
        private type t[n[+_], +a] = Free[n, a]
        override def lift[n[+_], a](n: n[a])(implicit i: Monad[n]): t[n, a] = Roll {
            import i.`for`
            for { a <- n } yield Pure(a)
        }
        override def liftWith[n[+_], a](f: Run => n[a])(implicit i: Monad[n]): t[n, a] = error("todo")

        implicit def _asMonadFree[f[+_]]: MonadFree[f, apply1[f]#apply1] = new MonadFree[f, apply1[f]#apply1] {
            private type m[+a] = Free[f, a]
            override def wrap[a](fs: f[m[a]]): m[a] = Roll(fs)
        }
    }

    // "unlift"
    def retract[f[+_], a](fr: Free[f, a])(implicit i: Monad[f]): f[a] = fr match {
        case Pure(a) => i.`return`(a)
        case Roll(fs) => {
            import i.`for`
            for { a <- fs } { retract(a) }
        }
    }

    trait MonadFree[f[+_], m[+_]] {
        def wrap[a](fs: f[m[a]]): m[a]
    }

    object MonadFree {
        def apply[f[+_], m <: Kind.Function1](implicit i: MonadFree[f, m#apply1]): MonadFree[f, m#apply1] = i

        implicit def _ofReaderT[f[+_], n[+_], e](implicit i: Functor[f], j: MonadFree[f, n]): MonadFree[f, ({type L[+a] = ReaderT[e, n, a]})#L] = new MonadFree[f, ({type L[+a] = ReaderT[e, n, a]})#L] {
            private type m[+a] = ReaderT[e, n, a]
            override def wrap[a](fs: f[m[a]]): m[a] = ReaderT { (e: e) => j.wrap {
                import i.`for`
                for { f <- fs } yield ReaderT.run(f)(e)
            } }
        }
    }

    // ContT where `r` varies.
    trait Codensity[f[+_], +a] {
        def apply[r](k: a => f[r]): f[r]
    }

    object Codensity extends MonadTrans[Codensity] {
        trait apply1[f[+_]] extends Kind.Function1 {
            override type apply1[+a] = Codensity[f, a]
        }

        implicit def _asMonad[f[+_]]: Monad[apply1[f]#apply1] = new Monad[apply1[f]#apply1] {
            private type m[+a] = Codensity[f, a]
            override def `return`[a](x: Lazy[a]): m[a] = new Codensity[f, a] {
                override def apply[r](k: a => f[r]): f[r] = k(x)
            }
            override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = new Codensity[f, b] {
                override def apply[r](c: b => f[r]): f[r] = m(a => k(a)(c))
            }
        }

        // as MonadTrans
        private type t[n[+_], a] = Codensity[n, a]
        override def lift[n[+_], a](n: n[a])(implicit i: Monad[n]): t[n, a] = new Codensity[n, a] {
            override def apply[r](k: a => n[r]): n[r] = i.op_>>=(n)(k)
        }
        override def liftWith[n[+_], a](f: Run => n[a])(implicit i: Monad[n]): t[n, a] = error("todo")

        implicit def _asMonadFree[f[+_], n[+_]](implicit i: MonadFree[f, n], j: Functor[f]): MonadFree[f, apply1[n]#apply1] = new MonadFree[f, apply1[n]#apply1] {
            private type m[+a] = Codensity[n, a]
            override def wrap[a](t: f[m[a]]): m[a] = new Codensity[n, a] {
                override def apply[r](h: a => n[r]): n[r] = i.wrap {
                    import j.`for`
                    for { p <- t } yield p(h)
                }
            }
        }
    }

    def join[f[+_], a](fr: Free[f, Free[f, a]])(implicit i: Functor[f]): Free[f, a] = fr match {
        case Pure(a) => a
        case Roll(as/*: f[Free[f, Free[f, a]]]*/) => Roll {
            import i.`for`
            for { a/*: Free[f, Free[f, a]]*/ <- as } yield join(a)
        }
    }

    final case class Bin[+a](left: a, right: a)

    object Bin {
    }

    type Tree[+a] = Free[Bin, a]

    def bin[m[+_], a](l: m[a])(r: m[a])(implicit i: MonadFree[Bin, m]): m[a] = i.wrap(Bin(l, r))

    def testTrivial {
        implicit val mf = MonadFree[Bin, Free.apply1[Bin]]
        val b = bin[Free.apply1[Bin]#apply1, Int](Pure(1))(Roll(Bin(Pure(1), Pure(2))))
        println(b)
    }

}
