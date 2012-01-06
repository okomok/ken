

// Public domain


package com.github.okomok.kentest.example


import com.github.okomok.ken._


// http://comonad.com/reader/2008/monads-for-free/


class MonadsForFreeTest extends org.scalatest.junit.JUnit3Suite {


    sealed abstract class Free[+f[+_], +a]
    final case class Return[a](_1: a) extends Free[Nothing, a]
    final case class Roll[f[+_], a](_1: f[Free[f, a]]) extends Free[f, a]

    object Free {
        trait apply1[f[+_]] extends Kind.Function1 {
            override type apply1[+a] = Free[f, a]
        }

        implicit def _asMonad[f[+_]](implicit i: Functor[f]): Monad[({type m[+a] = Free[f, a]})#m] = new Monad[({type m[+a] = Free[f, a]})#m] {
            private type m[+a] = Free[f, a]
            override def `return`[a](x: Lazy[a]): m[a] = Return(x)
            override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = m match {
                case Return(a) => k(a)
                case Roll(m) => Roll { i.fmap((f: Free[f, a]) => f >>= k)(m) }
            }
        }
    }


    def foldF[f[+_], a](phi: f[a] => a)(m: Free[f, a])(implicit i: Functor[f]): a = m match {
        case Roll(x) => phi { i.fmap(foldF(phi))(x) }
        case Return(x) => x
    }

    trait Unforall[f[+_]] {
        def apply[a]: f[a]
    }

    final case class Forall[f[+_]](unforall: Unforall[f])

    def cataF[f[+_], a](phi: f[a] => a)(k: Forall[({type m[+a] = Free[f, a]})#m])(implicit i: Functor[f]): a = {
        foldF(phi)(k.unforall.apply[a])
    }


    final case class Succ[+a](_1: a)

    object Succ extends Functor[Succ] with ThisIsInstance {
        private type f[+a] = Succ[a]
        override def fmap[a, b](f: a => b): f[a] => f[b] = {
            case Succ(a) => Succ(f(a))
        }
    }


    type Peano = Forall[({type m[+a] = Free[Succ, a]})#m]


    lazy val sfm = Monad[Free.apply1[Succ]]
    import sfm.>>

    def testTrivial {
        val x = Roll(Succ(Roll(Succ(Return())))) >> Roll(Succ(Return()))
        expect( Roll(Succ(Roll(Succ(Roll(Succ(Return())))))) )(x)
        expect(x)(toNat(3))
        expect(3)(toInt(x))
        expect(3)(toInt_(x))
    }


    lazy val toNat: Int => Free[Succ, Unit] = {
        case n if n > 0 => toNat(n - 1) >> Roll(Succ(Return()))
        case _ => Return()
    }

    lazy val toInt: Free[Succ, Any] => Int = m => {
        val phi: Succ[Int] => Int = { case Succ(n) => n + 1 }
        foldF(phi)(sfm.fmap(const(0))(m))
    }

    def cata_[f[+_], a](phi: f[a] => a)(z: a)(k: Exists[({type m[+a] = Free[f, a]})#m])(implicit i: Functor[f]): a = {
        foldF(phi)(Monad[Free.apply1[f]].fmap(const(z))(k.unexists))
    }


    final case class Exists[f[+_]](unexists: f[_])

    lazy val toInt_ : Free[Succ, Any] => Int = m => {
        val phi: Succ[Int] => Int = { case Succ(n) => n + 1 }
        cata_(phi)(0)( Exists[({type m[+a] = Free[Succ, a]})#m](m) )
    }

}
