

// Public domain


package com.github.okomok.kentest.example


import com.github.okomok.ken._


// http://d.hatena.ne.jp/einblicker/20111123/1322042879
// https://github.com/scalaz/scalaz/blob/master/core/src/main/scala/scalaz/Forall.scala


class ForallTest extends org.scalatest.junit.JUnit3Suite {

    trait Forall[p[_]] {
        def apply[a]: p[a]
    }

    type Not[a] = a => Nothing
    type Dne[p[_]] = Not[p[a]] forSome { type a }
    type Cps[p[_]] = Not[Dne[p]]

    def fromCps[p[_]](p: Cps[p]): Forall[p] = new Forall[p] {
        override def apply[a]: p[a] = {
            case class Control(arg: p[a]) extends Throwable
            try {
                val f: p[a] => Nothing = arg => throw new Control(arg)
                p(f)
            } catch {
                case Control(arg) => arg
            }
        }
    }

    def testTrivial {
        // Nil is the evidence why any type can be a List.
        val g = fromCps[List] { f => f(Nil) }
        val x: List[Int] = g.apply[Int]
        expect(Nil)(x)
    }

}
