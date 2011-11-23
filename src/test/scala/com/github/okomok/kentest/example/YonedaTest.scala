

// Public domain


package com.github.okomok.kentest.example


import com.github.okomok.ken._


// http://comonad.com/reader/2011/free-monads-for-less-2/


class YonedaTest extends org.scalatest.junit.JUnit3Suite {

    // Recall the Codensity.
    trait Codensity[f[+_], +a] {
        def apply[r](k: a => f[r]): f[r]
    }

    trait Yoneda[f[+_], +a] {
        def apply[r](k: a => r): f[r]
    }

    object Yoneda {
        trait apply1[f[+_]] extends Kind.Function1 {
            override type apply1[+a] = Yoneda[f, a]
        }

        // Yoneda.apply1[g] is always a Functor, regardless of what g is.
        implicit def _asFunctor[g[+_]]: Functor[apply1[g]#apply1] = new Functor[apply1[g]#apply] {
            private type f[+a] = Yoneda[g, a]
            override def fmap[a, b](f: a => b): f[a] => f[b] = m => new Yoneda[g, b] {
                override def apply[r](k: b => r): g[r] = m(k `.` f)
            }
        }
    }

    def liftYoneda[f[+_], a](a: f[a])(implicit i: Functor[f]): Yoneda[f, a] = new Yoneda[f, a] {
        override def apply[r](f: a => r): f[r] = i.fmap(f)(a)
    }

    def lowerYoneda[f[+_], a](f: Yoneda[f, a]): f[a] = f(id)

    trait YMaybe[+a] {
        def apply[r](k: a => r): r => r
    }

    final case class GEndo[m[+_], r](override val old: m[r] => m[r]) extends NewtypeOf[m[r] => m[r]]

    final case class Rec[f[+_], r](override val old: (f[r] => r) => r) extends NewtypeOf[(f[r] => r) => r]

    // equivalent to Yoneda[Rec.apply1[f]#apply1, a]
    trait F[f[+_], a] {
        def apply[r](k: a => r): (f[r] => r) => r
    }

    // given up.


    def testTrivial {
    }

}
