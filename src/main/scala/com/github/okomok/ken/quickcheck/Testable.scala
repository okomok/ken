

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2000-2006, Koen Claessen
// Copyright (c) 2006, Bjorn Bringert
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package quickcheck


trait Testable[prop] extends Typeclass0[prop] {
    final val asTestable: Testable[prop] = this

    type property = prop => Property
    def property: property

    type mapResult = (Result => Result) => prop => Property
    def mapResult: mapResult = f => {
        val wrap: (Result => Result) => Result => IO[Result] = f => res => for {
            mres <- tryEvaluate(res)
        } yield {
            f {
                mres match {
                    case Left(err) => Result.exception(err)
                    case Right(res) => res
                }
            }
        }
        import IO.>>=
        mapIOResult(io => io >>= wrap(f))
    }

    type mapIOResult = (IO[Result] => IO[Result]) => prop => Property
    def mapIOResult: mapIOResult = f => {
        val wrap: IO[Result] => IO[Result] = iores => for {
            miores <- tryEvaluate(iores)
            * <- miores match {
                case Left(err) => IO.`return`(Result.exception(err))
                case Right(iores) => iores
            }
        } yield *
        mapRoseIOResult(Rose.fmap(f `.` wrap))
    }

    type mapRoseIOResult = (Rose[IO[Result]] => Rose[IO[Result]]) => prop => Property
    def mapRoseIOResult: mapRoseIOResult = f => mapProp({ case Prop(t) => Prop(f(t)) })

    type mapProp = (Prop => Prop) => prop => Property
    def mapProp: mapProp = f => p => Gen.fmap(f)(property(p))

    type mapSize = (Int => Int) => prop => Property
    def mapSize: mapSize = f => p => Gen.sized(n => Gen.resize(f(n))(property(p)))

    def shrinking[a](shrink: a => List[a])(x: a)(pf: a => prop): Property = {
        lazy val props: a => Rose[Property] = x => Rose(property(pf(x)), for { x_ <- shrink(x) } yield props(x_))
        Gen.fmap((ps: Rose[Prop]) => Prop(Rose.join(Rose.fmap(Prop.unProp)(ps))))(Gen.promote(props(x)))
    }

    type callback = Callback => prop => Property
    def callback: callback = cb => mapResult(res => res.copy(callbacks = cb :: Result.callbacks(res)))

    type whenFail = IO[Unit] => prop => Property
    def whenFail: whenFail = m => callback { PostFinalFailure { st => res => m } }

    type `whenFail'` = IO[Unit] => prop => Property
    def `whenFail'`: `whenFail'` = m => callback { PostTest { st => res =>
        if (Result.ok(res) == Just(False)) m else IO.`return`()
    } }

    type expectFailure = prop => Property
    def expectFailure: expectFailure = mapResult(res => res.copy(expect = False))

    type label = String => prop => Property
    def label: label = s => classify(True)(s)

    def collect[a](x: a): prop => Property = label(Show.show(x))

    type classify = Bool => String => prop => Property
    def classify: classify = b => s => cover(b)(0)(s)

    type cover = Bool => Int => String => prop => Property
    def cover: cover = b => n => s => mapIOResult { ior =>
        for {
            eeb <- tryEvaluate(b)
            res <- ior
        } yield {
            eeb match {
                case Left(err) => {
                    @Annotation.CaseClassCopyWorkaround
                    val tmp = "Exception: \'" ++: Str.showErr(err) ++: List.from("\'")
                    res.copy(ok = Just(False), reason = tmp)
                }
                case Right(True) => res.copy(stamp = (s, n) :: Result.stamp(res))
                case Right(False) => res
            }
        }
    }

    type op_==> = Bool => prop => Property
    def op_==> : op_==> = {
        case False => _ => Testable.property()
        case True => p => property(p)
    }

    def forAll[a](gen: Gen[a])(pf: a => prop): Property = {
        import Gen.>>=
        gen >>= { (x: a) =>
            Testable.ofProperty.whenFail(IO.putStrLn(Show.show(x))) {
                property(pf(x))
            }
        }
    }

    def forAllShrink[a](gen: Gen[a])(shrink: a => List[a])(pf: a => prop): Property = {
        import Gen.>>=
        gen >>= { (x: a) =>
            Testable.ofProperty.shrinking(shrink)(x) { (x_ : a) =>
                Testable.ofProperty.whenFail(IO.putStrLn(Show.show(x_))) {
                    property(pf(x_))
                }
            }
        }
    }


}


trait TestableProxy[prop] extends Testable[prop] {
    def selfTestable: Testable[prop]

    def property: property = selfTestable.property

    // TODO
}


object Testable extends TestableInstance with TestableShortcut {
    def apply[prop <: Kind.Function0](implicit i: Testable[prop#apply0]): Testable[prop#apply0] = i

    val ofProperty: Testable[Property] = ofGen[Prop]
}


sealed trait TestableInstance { this: Testable.type =>
    implicit val ofUnit: Testable[Unit] = new Testable[Unit] {
        override val property: property = _ => Result.property(Result.rejected)
    }

    implicit val ofBool: Testable[Bool] = new Testable[Bool] {
        override val property: property = x => ofProperty.property(Property.liftBool(x))
    }

    implicit def ofGen[prop](implicit i: Testable[prop]): Testable[Gen[prop]] = new Testable[Gen[prop]] {
        override val property: property = mp => for { p <- mp; * <- i.property(p) } yield *
    }

    implicit def ofFunction[a, prop](implicit i: Testable[prop], j: Arbitary[a]): Testable[a => prop] = new Testable[a => prop] {
        override val property: property = f => i.forAllShrink(j.arbitary)(j.shrink)(f)
    }
}


sealed trait TestableShortcut { this: Testable.type =>
    def property[prop](prop: prop)(implicit i: Testable[prop]): Property = i.property(prop)

    // TODO
}
