

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2000-2006, Koen Claessen
// Copyright (c) 2006, Bjorn Bringert
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package quickcheck


final case class PropertyM[n[+_], +a](override val old: (a => Gen[n[Property]]) => Gen[n[Property]]) extends NewtypeOf[(a => Gen[n[Property]]) => Gen[n[Property]]] with ((a => Gen[n[Property]]) => Gen[n[Property]]) {
    override def apply(x: a => Gen[n[Property]]): Gen[n[Property]] = old(x)
}


object PropertyM extends Kind.FunctionLike {
    trait apply[n[+_]] extends apply1[n]
    trait apply1[n[+_]] extends Kind.Function1 {
        override type apply1[+a] = PropertyM[n, a]
    }

    implicit def _asMonad[n[+_]]: Monad[apply1[n]#apply1] = new Monad[apply1[n]#apply1] {
        // Functor
        private type f[+a] = PropertyM[n, a]
        override def fmap[a, b](f: a => b): f[a] => f[b] = m => PropertyM(k => (m.old)(k `.` f))
        // Monad
        private type m[+a] = PropertyM[n, a]
        override def `return`[a](x: Lazy[a]): m[a] = PropertyM(k => k(x))
        override def op_>>=[a, b](m: m[a])(f: a => m[b]): m[b] = PropertyM(k => (m.old)(a => (f(a).old)(k)))
    }

    def assert[n[+_]](b: Bool)(implicit i: Monad[n]): PropertyM[n, Unit] = PropertyM { k =>
        if (b) k()
        else Gen.`return`(i.`return`(Testable.property(False)))
    }

    def run[n[+_], a](n: n[a])(implicit i: Monad[n]): PropertyM[n, a] = PropertyM { k =>
        implicit val fm = Monad[Function.apply[a]]
        import i.>>=
        for { x <- Gen.promote[fm.apply, n[Property]](k)(fm) } yield (n >>= x)
    }

    def pick[n[+_], a](gen: Gen[a])(implicit i: Monad[n], j: Show[a]): PropertyM[n, a] = PropertyM { k =>
        for {
            a <- gen
            mp <- k(a)
        } yield {
            import i.`for`
            for {
                p <- mp
            } yield {
                Testable.forAll(Gen.`return`(a))(const(p))
            }
        }
    }

    def wp[n[+_], a, b](n: n[a])(k: (a => PropertyM[n, b]))(implicit i: Monad[n]): PropertyM[n, b] = {
        val pm = Monad[apply1[n]]
        import pm.>>=
        run(n) >>= k
    }

    def forAllM[n[+_], a, b](gen: Gen[a])(k: a => PropertyM[n, b])(implicit i: Monad[n], j: Show[a]): PropertyM[n, b] = {
        val pm = Monad[apply1[n]]
        import pm.>>=
        pick(gen) >>= k
    }

    def monitor[n[+_]](f: Property => Property)(implicit i: Monad[n]): PropertyM[n, Unit] = PropertyM { k =>
        Gen.fmap(i.liftM(f))(k())
    }

    def monadic[n[+_], a](run: n[Property] => Property)(p: PropertyM[n, a])(implicit i: Monad[n]): Property = {
        for {
            mp <- (p.old)(const(Gen.`return`(i.`return`(Testable.property(True)))))
        } {
            run(mp)
        }
    }

    def monadicIO[n[+_], a](p: PropertyM[IO, a]): Property = Testable.property {
        Gen.fmap(IO.unsafePerformIO[Property])((p.old)(const(Gen.`return`(IO.`return`(Testable.property(True))))))
    }
}
