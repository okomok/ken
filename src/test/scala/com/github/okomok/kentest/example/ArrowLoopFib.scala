
package com.github.okomok.kentest.example

// https://gist.github.com/1570459
//   by akihiro4chawon

import com.github.okomok.ken

object ArrowLoopFib extends ken.Main {
    import ken._
    import List.{tail, zipWith}
    import Tuple2.snd

    val fibSeqFrom = locally {
        // use Function Arrow in this environment
        import Function.{app, loop, &&&:, ***:, <<<:, >>>:}

        val `(.)` = `op_.`[Lazy[List[Int]], Lazy[List[Int]], Lazy[List[Int]]]_
        val `(:)`: Int => Lazy[List[Int]] => Lazy[List[Int]] = List.op_::[Int]_
        val `((.).(:))` = `(.)` `.` `(:)`

        val tupleToHeads = (`((.).(:))` ***: `(:)`) >>>: app
        val sumAdjacentTerms = (Lazy.liftM2(zipWith[Int, Int, Int](Int.op_+))_ &&&: Lazy.liftM(tail[Int])_) >>>: app
        val fibLoop = tupleToHeads ***: sumAdjacentTerms >>>: app

        loop(fibLoop.&&&:(snd _)) // Dont be confused with `fibLoop &&&: (snd _)`!
        // The expression above is equivalent to the below except that type inference works.
        // `loop(snd[Lazy[List[Int]]]_ &&&: fibLoop)`
    }

    override def main_ = {
        // use Kleisli Arrow in this environment
        val kio = Arrow[Kleisli.apply[IO.type]]
        import kio.{arr, >>>:}

        val printFirst100FibNumFrom =
            arr(fibSeqFrom) >>>: arr(List.take[Int](100)_) >>>: Kleisli(IO.print(_: List[Int]))

        printFirst100FibNumFrom((0, 1))
    }

    // def main(args: Array[java.lang.String]) {
    // val fibSeq = fibSeqFrom((0, 1))
    // println(List.take(100)(fibSeq))
    // }
}
