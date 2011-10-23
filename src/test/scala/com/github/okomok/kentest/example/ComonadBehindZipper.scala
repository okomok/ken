

// Public domain


package com.github.okomok.kentest.example


import com.github.okomok.ken._


// See: http://blog.sigfpe.com/2007/01/monads-hidden-behind-every-zipper.html


class ComonadBehindZipperTest extends org.scalatest.junit.JUnit3Suite {

    // aN ... a1 a0 ! b0 b1 b2 ... bN
    final case class Zipper[+a](val _1: List[a], val _2: List[a])
        extends Product2[List[a], List[a]] // will be unneeded soon.
        with Deriving[Eq ^:: Show ^:: Kind.Nil]

    object Zipper extends Comonad[Zipper] with ThisIsInstance {
        def left[a](w: Zipper[a]): Zipper[a] = (w: @unchecked) match {
            case Zipper(a :: as, bs) => Zipper(as, a :: bs)
        }

        def right[a](w: Zipper[a]): Zipper[a] = (w: @unchecked) match {
            case Zipper(as, b :: bs) => Zipper(b :: as, bs)
        }

        // Functor
        private type f[+a] = Zipper[a]
        override def fmap[a, b](f: a => b): f[a] => f[b] = {
            case Zipper(as, bs) => Zipper(List.fmap(f)(as), List.fmap(f)(bs))
        }
        // Extend
        private type w[+a] = Zipper[a]
        override def duplicate[a](w: w[a]): w[w[a]] = Zipper(List.tail(List.iterate(left[a])(w)), List.iterate(right[a])(w))
        // Comonad
        override def extract[a](w: w[a]): a = (w: @unchecked) match {
            case Zipper(_, b :: _) => b
        }
    }

    def testTrivial {
        // 0 ... 0 0 ! 0 1 2 3 0 0 ... 0
        val a: Zipper[Int] = Zipper(List.repeat(0), List(0,1,2,3) ++: List.repeat(0))

        val f: Zipper[Int] => Int = w => (w: @unchecked) match {
            case Zipper(a :: _, b !:: c :: _) => a + (2 * b) + c
        }

        val test = {
            val Zipper(u, v) = Zipper.extend(f)(a)
            List.take(5)(v)
        }

        expect(List(1,4,8,8,3))(test)
    }
}
