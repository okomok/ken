

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2000-2006, Koen Claessen
// Copyright (c) 2006, Bjorn Bringert
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package quickcheck


trait Testable[prop] extends Typeclass[prop] {
    final val asTestable: Testable[prop] = this

    // Core
    //
    type property = prop => Property
    def property: property

    // Extra
    //
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
        mapIOResult(io => io >>= wrap(f))
    }

    type mapIOResult = (IO[Result] => IO[Result]) => prop => Property
    def mapIOResult: mapIOResult = f => {
        val wrap: IO[Result] => IO[Result] = iores => for {
            miores <- tryEvaluate(iores)
        } {
            miores match {
                case Left(err) => IO.`return`(Result.exception(err))
                case Right(iores) => iores
            }
        }
        mapRoseIOResult(Rose.fmap(f `.` wrap))
    }

    type mapRoseIOResult = (Rose[IO[Result]] => Rose[IO[Result]]) => prop => Property
    def mapRoseIOResult: mapRoseIOResult = f => mapProp({ case Prop(t) => Prop(f(t)) })

    type mapProp = (Prop => Prop) => prop => Property
    def mapProp: mapProp = f => p => Gen.fmap(f)(property(p))

    type mapSize = (Int => Int) => prop => Property
    def mapSize: mapSize = f => p => Gen.sized(n => Gen.resize(f(n))(property(p)))

    def shrinking[a](shrink: a => List[a])(a: a)(pf: a => prop): Property = {
        lazy val props: a => Rose[Property] = x => Rose(property(pf(x)), for { x_ <- shrink(x) } yield props(x_))
        Gen.fmap((ps: Rose[Prop]) => Prop(Rose.join(Rose.fmap(Prop.unProp)(ps): Rose[Rose[IO[Result]]]): Rose[IO[Result]]))(Gen.promote(props(a)): Gen[Rose[Prop]])
    }

    type callback = Callback => prop => Property
    def callback: callback = cb => mapResult(res => res.copy(callbacks = cb :: res.callbacks))

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

    def collect[a](x: a)(implicit i: Show[a]): prop => Property = label(Show.show(x))

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
                    @Annotation.caseClassCopyWorkaround
                    val tmp = "Exception: \'" ++: Str.showErr(err) ++: List.from("\'")
                    res.copy(ok = Just(False), reason = tmp)
                }
                case Right(True) => res.copy(stamp = (s, n) :: Result.stamp(res))
                case Right(False) => res
            }
        }
    }

    type op_==>: = Bool => prop => Property
    def op_==>: : op_==>: = {
        case False => _ => Testable.property()
        case True => p => property(p)
    }

    def forAll[a](gen: Gen[a])(pf: a => prop)(implicit i: Show[a]): Property = {
        gen >>= { (x: a) =>
            Testable.ofProperty.whenFail(IO.putStrLn(Show.show(x))) {
                property(pf(x))
            }
        }
    }

    def forAllShrink[a](gen: Gen[a])(shrink: a => List[a])(pf: a => prop)(implicit i: Show[a]): Property = {
        gen >>= { (x: a) =>
            Testable.ofProperty.shrinking(shrink)(x) { (x_ : a) =>
                Testable.ofProperty.whenFail(IO.putStrLn(Show.show(x_))) {
                    property(pf(x_))
                }
            }
        }
    }

    def op_:&:[prop2](p1: prop)(p2: prop2)(implicit j: Testable[prop2]): Property = {
        Arbitary._ofBool.arbitary >>= { (b: Bool) =>
            Testable.ofProperty.whenFail(IO.putStrLn(if (b) "LHS" else "RHS")) {
                if (b) property(p1) else j.property(p2)
            }
        }
    }

    // Operators
    //
    private[quickcheck] sealed class Op_==>:(b: Bool) {
        def ==>:(p: prop): Property = op_==>:(b)(p)
    }
    final implicit def ==>:(b: Bool): Op_==>: = new Op_==>:(b)

    private[quickcheck] sealed class Op_:&:(p1: prop) {
        def :&:[prop2](p2: prop2)(implicit j: Testable[prop2]): Property = op_:&:(p1)(p2)
    }
    final implicit def ==>:(p1: prop): Op_:&: = new Op_:&:(p1)

    // Test
    //
    type quickCheck = prop => IO[Unit]
    def quickCheck: quickCheck = p => quickCheckWith(Test.stdArgs)(p)

    type quickCheckWith = Test.Args => prop => IO[Unit]
    def quickCheckWith: quickCheckWith = args => p => {
        quickCheckWithResult(args)(p) >> IO.`return`()
    }

    type quickCheckWithResult = Test.Args => prop => IO[Test.Result]
    def quickCheckWithResult: quickCheckWithResult = args => p => for {
        tm <- Str.newTerminal
        rnd <- args.replay match {
            case Nothing => IO.newStdGen
            case Just((rnd, _)) => IO.`return`(rnd)
        }
    } {
        Test.test(
            State(
                terminal = tm,
                maxSuccessTests = args.maxSuccess,
                maxDiscardedTests = args.maxDiscard,
                computeSize = args.replay match {
                    case Nothing => n => d => {
                        import Int._div_
                        ((n * args.maxSize) _div_ args.maxSuccess) + (d _div_ 10)
                    }
                    case Just((_, s)) => _ => _ => s
                },
                numSuccessTests = 0,
                numDiscardedTests = 0,
                collected = Nil,
                expectedFailure = False,
                randomSeed = rnd,
                isShrinking = False,
                numSuccessShrinks = 0,
                numTryShrinks = 0
            )
        )(property(p).get)
    }
}


trait TestableProxy[prop] extends Testable[prop] {
    type selfTestable = Testable[prop]
    def selfTestable: selfTestable

    override def property: property = selfTestable.property

    override def mapResult: mapResult = selfTestable.mapResult
    override def mapIOResult: mapIOResult = selfTestable.mapIOResult
    override def mapRoseIOResult: mapRoseIOResult = selfTestable.mapRoseIOResult
    override def mapProp: mapProp = selfTestable.mapProp
    override def mapSize: mapSize = selfTestable.mapSize
    override def shrinking[a](shrink: a => List[a])(a: a)(pf: a => prop): Property = selfTestable.shrinking(shrink)(a)(pf)
    override def callback: callback = selfTestable.callback
    override def whenFail: whenFail = selfTestable.whenFail
    override def `whenFail'`: `whenFail'` = selfTestable.`whenFail'`
    override def expectFailure: expectFailure = selfTestable.expectFailure
    override def label: label = selfTestable.label
    override def collect[a](x: a)(implicit i: Show[a]): prop => Property = selfTestable.collect(x)(i)
    override def classify: classify = selfTestable.classify
    override def cover: cover = selfTestable.cover
    override def op_==>: : op_==>: = selfTestable.op_==>:
    override def forAll[a](gen: Gen[a])(pf: a => prop)(implicit i: Show[a]): Property = selfTestable.forAll(gen)(pf)(i)
    override def forAllShrink[a](gen: Gen[a])(shrink: a => List[a])(pf: a => prop)(implicit i: Show[a]): Property = selfTestable.forAllShrink(gen)(shrink)(pf)(i)
    override def op_:&:[prop2](p1: prop)(p2: prop2)(implicit j: Testable[prop2]): Property = selfTestable.op_:&:(p1)(p2)(j)

    override def quickCheck: quickCheck = selfTestable.quickCheck
    override def quickCheckWith: quickCheckWith = selfTestable.quickCheckWith
    override def quickCheckWithResult: quickCheckWithResult = selfTestable.quickCheckWithResult
}


object Testable extends TestableInstance with TestableShortcut {
    def apply[prop <: Kind.Function0](implicit i: Testable[prop#apply0]): Testable[prop#apply0] = i

    val ofProperty: Testable[Property] = _ofGen[Prop]
}


sealed trait TestableInstance { this: Testable.type =>
    implicit val _ofUnit: Testable[Unit] = new Testable[Unit] {
        override val property: property = _ => Result.property(Result.rejected)
    }

    implicit val _ofBool: Testable[Bool] = new Testable[Bool] {
        override val property: property = x => ofProperty.property(Property.liftBool(x))
    }

    implicit def _ofGen[prop](implicit i: Testable[prop]): Testable[Gen[prop]] = new Testable[Gen[prop]] {
        override val property: property = mp => for { p <- mp } { i.property(p) }
    }

    implicit def _ofFunction[a, prop](implicit i: Testable[prop], j: Arbitary[a], k: Show[a]): Testable[a => prop] = new Testable[a => prop] {
        override val property: property = f => i.forAllShrink(j.arbitary)(j.shrink)(f)
    }
}


trait TestableShortcut {
    def property[prop](prop: prop)(implicit i: Testable[prop]): Property = i.property(prop)

    def mapResult[prop](f: Result => Result)(prop: prop)(implicit i: Testable[prop]): Property = i.mapResult(f)(prop)
    def mapIOResult[prop](f: IO[Result] => IO[Result])(prop: prop)(implicit i: Testable[prop]): Property = i.mapIOResult(f)(prop)
    def mapRoseIOResult[prop](f: Rose[IO[Result]] => Rose[IO[Result]])(prop: prop)(implicit i: Testable[prop]): Property = i.mapRoseIOResult(f)(prop)
    def mapProp[prop](f: Prop => Prop)(prop: prop)(implicit i: Testable[prop]): Property = i.mapProp(f)(prop)
    def mapSize[prop](f: Int => Int)(prop: prop)(implicit i: Testable[prop]): Property = i.mapSize(f)(prop)
    def shrinking[prop, a](shrink: a => List[a])(a: a)(pf: a => prop)(implicit i: Testable[prop]): Property = i.shrinking(shrink)(a)(pf)
    def callback[prop](cb: Callback)(prop: prop)(implicit i: Testable[prop]): Property = i.callback(cb)(prop)
    def whenFail[prop](m: IO[Unit])(prop: prop)(implicit i: Testable[prop]): Property = i.whenFail(m)(prop)
    def `whenFail'`[prop](m: IO[Unit])(prop: prop)(implicit i: Testable[prop]): Property = i.`whenFail'`(m)(prop)
    def expectFailure[prop](prop: prop)(implicit i: Testable[prop]): Property = i.expectFailure(prop)
    def label[prop](s: String)(prop: prop)(implicit i: Testable[prop]): Property = i.label(s)(prop)
    def collect[prop, a](x: a)(prop: prop)(implicit i: Testable[prop], j: Show[a]): Property = i.collect(x)(j)(prop)
    def classify[prop](b: Bool)(s: String)(prop: prop)(implicit i: Testable[prop]): Property = i.classify(b)(s)(prop)
    def cover[prop](b: Bool)(n: Int)(s: String)(prop: prop)(implicit i: Testable[prop]): Property = i.cover(b)(n)(s)(prop)
    def op_==>:[prop](b: Bool)(prop: prop)(implicit i: Testable[prop]): Property = i.op_==>:(b)(prop)
    def forAll[prop, a](gen: Gen[a])(pf: a => prop)(implicit i: Testable[prop], j: Show[a]): Property = i.forAll(gen)(pf)
    def forAllShrink[prop, a](gen: Gen[a])(shrink: a => List[a])(pf: a => prop)(implicit i: Testable[prop], j: Show[a]): Property = i.forAllShrink(gen)(shrink)(pf)(j)
    def op_:&:[prop, prop2](p1: prop)(p2: prop2)(implicit i: Testable[prop], j: Testable[prop2]): Property = i.op_:&:(p1)(p2)(j)

    def quickCheck[prop](prop: prop)(implicit i: Testable[prop]): IO[Unit] = i.quickCheck(prop)
    def quickCheckWith[prop](args: Test.Args)(prop: prop)(implicit i: Testable[prop]): IO[Unit] = i.quickCheckWith(args)(prop)
    def quickCheckWithResult[prop](args: Test.Args)(prop: prop)(implicit i: Testable[prop]): IO[Test.Result] = i.quickCheckWithResult(args)(prop)

    private[quickcheck] sealed class _Op_==>:(b: Bool) {
        def ==>:[prop](p: prop)(implicit i: Testable[prop]): Property = op_==>:(b)(p)
    }
    implicit def ==>:(b: Bool): _Op_==>: = new _Op_==>:(b)

    private[quickcheck] sealed class _Op_:&:[prop](p1: prop) {
        def :&:[prop2](p2: prop2)(implicit i: Testable[prop], j: Testable[prop2]): Property = op_:&:(p1)(p2)
    }
    implicit def :&:[prop](p1: prop): _Op_:&:[prop] = new _Op_:&:(p1)
}
