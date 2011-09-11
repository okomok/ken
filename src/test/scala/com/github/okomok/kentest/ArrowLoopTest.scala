

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class ArrowLoopTest extends org.scalatest.junit.JUnit3Suite {

    val fa = ArrowLoop[Function.type]

    def testFactorial {
        // See: https://gist.github.com/1022916

        val factorial = fa.loop[Int, Int, Int => Int] {
            case (b, d) => (
                Lazy { d.!(b) },
                Lazy {
                    case 0 => 1
                    case x => x * d.!(x - 1)
                }
            )
        }

        expect(List(1,1,2,6,24,120,720,5040,40320,362880))(List.map(factorial)(List.range(0, 10)))
    }

    def testRepeat {
        // See: http://d.hatena.ne.jp/MaD/20070818

        import fa.&&&:
        type r[x] = Tuple2[Int, Lazy[List[Int]]] => Lazy[List[Int]]
        val x: r[Int] = Pair.snd[Lazy[List[Int]]]_
        val y: r[Int] = Pair.uncurry(List.op_::[Int])
        val r = fa.loop(x &&&: y)
        expect(List(1,1,1,1,1))(List.take(5)(r(1)))
    }
}
