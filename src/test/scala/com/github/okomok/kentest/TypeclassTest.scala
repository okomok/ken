

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class TypeclassTest extends org.scalatest.junit.JUnit3Suite {
/*
    def testImplicitObsolete {
        val m = Function._monad[Int]
        import m.StateT
        val mp = m.StateT._asMonadReader[Int, Int]
        ()
    }

    def testImplicit2Obsolete {
        val m = Function._monad[Int]
        val m_ = implicitly[Monad[m.apply]]
        import m.StateT
        val sm = implicitly[Monad[({type m[+a] = StateT[Int, a]})#m]]
        ()
    }
*/
    def testMakeAndInfer {
        val fm = Monad[Function.apply[Int]]
        val im = Monad[Identity.type]
        val lm = Monad[List.type]
        val wim = Monad[WeakIdentity.type]
        val mit = Monad[Identity.ListT.type]

        def infer[m[+_]](m: Monad[m]) = ()

        infer(fm)
        infer(im)
        infer(lm)
        infer(wim)
        infer(mit)
    }

    def testImplicit {
        implicit val fm = Monad[Function.apply[Int]]
        implicit val im = Monad[Identity.type]
        implicit val lm = Monad[List.type]
        implicit val wim = Monad[WeakIdentity.type]
        implicit val mit = Monad[Identity.ListT.type]

        def infer[a, m[+_]](f: m[a])(implicit m: Monad[m]) = ()
        def makeM[a, m[+_]]: m[a] = throw new java.lang.Error

        ignore {
            infer(fm.infer(x => x.toString))//(fm)
            infer(id[fm.apply[Predef.String]](x => x.toString))//(fm)
            infer(makeM[Int, im.apply])
        }
    }

    def testMonadTrans {
        val mt1 = MonadTrans[IO.StateT.apply[Int]]
        val mt2 = MonadTrans[IO.LazyT.type]
    }

    def testImply1 {
        val wt1 = Monad.weak[IO.LazyT.type]
    }

    def testEq {
        val i = implicitly[Eq[Int]]
        val j = implicitly[Ord[Int]]
        val k = implicitly[Ix[Int]]
    }
}
