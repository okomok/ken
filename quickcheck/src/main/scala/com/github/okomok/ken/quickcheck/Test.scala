

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2000-2006, Koen Claessen
// Copyright (c) 2006, Bjorn Bringert
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package quickcheck


object Test {

    // Args
    //
    final case class Args(
        replay: Maybe[(StdGen, Int)],
        maxSuccess: Int,
        maxDiscard: Int,
        maxSize: Int
    )

    val replay: Args => Maybe[(StdGen, Int)] = as => as.replay
    val maxSucces: Args => Int = as => as.maxSuccess
    val maxDiscard: Args => Int = as => as.maxDiscard
    val maxSize: Args => Int = as => as.maxSize

    // Result
    //
    sealed abstract class Result {
        def labels: List[(String, Int)]
    }

    final case class Success(override val labels: List[(String, Int)]) extends Result
    final case class GaveUp(numTests: Int, override val labels: List[(String, Int)]) extends Result
    final case class Failure(usedSeed: StdGen, usedSize: Int, reason: String, override val labels: List[(String, Int)]) extends Result
    final case class NoExpectedFailure(override val labels: List[(String, Int)]) extends Result

    val labels: Result => List[(String, Int)] = r => r.labels
    val numTests: GaveUp => Int = r => r.numTests
    val usedSeed: Failure => StdGen = r => r.usedSeed
    val usedSize: Failure => Int = r => r.usedSize
    val reason: Failure => String = r => r.reason

    val isSuccess: Result => Bool = {
        case Success(_) => True
        case _ => False
    }

    val stdArgs: Args = Args(Nothing, 100, 500, 100)

    // main test loop
    //
    val test: State => (StdGen => Int => Prop) => IO[Result] = st => f => {
        if (st.numSuccessTests >= st.maxSuccessTests) doneTesting(st)(f)
        else if (st.numDiscardedTests >= st.maxDiscardedTests) giveUp(st)(f)
        else runATest(st)(f)
    }

    val doneTesting: State => (StdGen => Int => Prop) => IO[Result] = st => f => for {
        _ <- if (st.expectedFailure) Str.putPart(st.terminal)("+++ OK, passed " ++: Show.show(st.numSuccessTests) ++: List.from(" tests"))
             else Str.putPart(st.terminal)(Str.bold("*** Failed!") ++: " Passed " ++: Show.show(st.numSuccessTests) ++: List.from(" tests (expected failure)"))
        _ <- success(st)
    } yield {
        if (st.expectedFailure) Success(labels = summary(st))
        else NoExpectedFailure(labels = summary(st))
    }

    val giveUp: State => (StdGen => Int => Prop) => IO[Result] = st => f => for {
        _ <- Str.putPart(st.terminal)(Str.bold("*** Gave up!") ++: " Passed only " ++: Show.show(st.numSuccessTests) ++: List.from(" tests"))
        _ <- success(st)
    } yield GaveUp(numTests = st.numSuccessTests, labels = summary(st))

    val runATest: State => (StdGen => Int => Prop) => IO[Result] = st => f => {
        val (rnd1, rnd2) = RandomGen.split(st.randomSeed)
        for {
            _ <- Str.putTemp(st.terminal)(
                "(" ++:
                Str.number(st.numSuccessTests)("test") ++:
                List.concat {
                    for {
                        _ <- List(())
                        if st.numDiscardedTests > 0
                    } yield {
                        "; " ++: Show.show(st.numDiscardedTests) ++: List.from(" discarded")
                    }
                } ++:
                List.from(")") )
            size = st.computeSize(st.numSuccessTests)(st.numDiscardedTests)
            (res, ts) <- run(f(rnd1)(size).get)
        } {
            res.ok match {
                case Just(True) => {
                    test(
                        st.copy(
                            numSuccessTests = st.numSuccessTests + 1,
                            randomSeed = rnd2,
                            collected = res.stamp :: st.collected,
                            expectedFailure = res.expect
                        )
                    )(f)
                }
                case Nothing => {
                    test(
                        st.copy(
                            numDiscardedTests = st.numDiscardedTests + 1,
                            randomSeed = rnd2,
                            expectedFailure = res.expect
                        )
                    )(f)
                }
                case Just(False) => {
                    for {
                        _ <- if (res.expect) Str.putPart(st.terminal)(Str.bold("*** Failed! "))
                             else Str.putPart(st.terminal)("+++ OK, failed as expected. ")
                        _ <- Str.putTemp(st.terminal)(
                            Str.short(30)(res.reason)
                            ++: " (after "
                            ++: Str.number(st.numSuccessTests+1)("test")
                            ++: List.from(")...") )
                        _ <- foundFailure(st)(res)(ts)
                    } yield {
                        if (Bool.not(res.expect)) Success(labels = summary(st))
                         else Failure(
                             usedSeed = st.randomSeed,
                             usedSize = size,
                             reason = res.reason,
                             labels = summary(st) )
                    }
                }
            }
        }
    }

    val summary: State => List[(String, Int)] = st => {
        import Int._div_
        List.reverse(
        List.sort(
        List.map((ss: List[String]) => (List.head(ss), (List.length(ss) * 100) _div_ st.numSuccessTests))(
        List.group(
        List.sort {
            for {
                s <- st.collected
                s_ = for { (t, _) <- s } yield t
                if Bool.not(List.`null`(s_))
            } yield List.concat(List.intersperse(List.from(", "))(s_))
        }))))
    }

    val success: State => IO[Unit] = st => {
        import Int._div_

        val showP: Int => String = p => (if (p < 10) " " else "") ++: Show.show(p) ++: List.from("% ")

        val labels: List[String] = {
            List.reverse(
            List.sort(
            List.map((ss: List[String]) => showP((List.length(ss) * 100) _div_ st.numSuccessTests) ++: List.head(ss))(
            List.group(
            List.sort(
                for {
                    s <- st.collected
                    s_ = for { (t, 0) <- s } yield t
                    if Bool.not(List.`null`(s_))
                } yield List.concat(List.intersperse(List.from(", "))(s_))
            )))))
        }

        val first: Pair[String, Int] => Pair[String, Int] => Bool = {
            case (x, _) => { case (y, _) => x == y }
        }

        val maxi: List[(String, Int)] => List[(String, Int)] = xs => {
            List.map((lps: List[(String, Int)]) => (Pair.fst(List.head(lps)), List.maximum(List.map(Pair.snd[Int])(lps))))(
            List.groupBy(first)(
            List.sort(xs)
            ))
        }

        val covers: List[String] = for {
            lps <-
                List.groupBy(first)(
                List.sort(
                    for {
                        lps <- st.collected
                        lp <- maxi(lps)
                        if Pair.snd(lp) > 0
                    } yield lp
                ))
                occurP = (100 * List.length(lps)) _div_ st.maxSuccessTests
                reqP = List.maximum(List.map(Pair.snd[Int])(lps))
                if occurP < reqP
        } yield {
            "only " ++: Show.show(occurP) ++: "% " ++: Pair.fst(List.head(lps)) ++: "; not" ++: Show.show(reqP) ++: List.from("%")
        }

        (labels ++: covers) match {
            case Nil => Str.putLine(st.terminal)(".")
            case pt !:: Nil => Str.putLine(st.terminal)(" (" ++: List.dropWhile(Char.isSpace)(pt) ++: List.from(")."))
            case cases => for {
                _ <- Str.putLine(st.terminal)(":")
            } {
                List.sequence__( for { pt <- cases } yield Str.putLine(st.terminal)(pt) )
            }
        }
    }

    val run: Rose[IO[quickcheck.Result]] => IO[(quickcheck.Result, List[Rose[IO[quickcheck.Result]]])] = rose => {
        val errResult: quickcheck.Result => SomeException => quickcheck.Result = res => err => {
            @Annotation.caseClassCopyWorkaround
            val tmp = "Exception: \'" ++: Str.showErr(err) ++: List.from("\'")
            res.copy(reason = tmp)
        }
        val errRose: SomeException => Rose[IO[quickcheck.Result]] = err => Rose(IO.`return`(errResult(Result.failed)(err)), Nil)

        def orElseErr[a](m: IO[a]): (String, SomeException => a) => IO[a] = {
            case (s, f) => for {
                eex <- tryEvaluateIO(m)
            } {
                eex match {
                    case Left(err) => IO.`return`(f(err))
                    case Right(x) => IO.`return`(x)
                }
            }
        }

        val strictOk: quickcheck.Result => quickcheck.Result = res => seq(res.ok == Just(False))(res)
        val repairList: List[Rose[IO[quickcheck.Result]]] => IO[List[Rose[IO[quickcheck.Result]]]] = xs => IO.`return`(xs)

        for {
            Rose(mres, ts) <- orElseErr(IO.`return`(rose))("rose", errRose)
            res <- orElseErr(mres)("mres", errResult(Result.failed))
            res_ <- orElseErr(IO.`return`(strictOk(res)))("ok", errResult(res.copy(ok = Just(False))))
            ts_ <- repairList(ts)
        } yield (res_, ts_)
    }

    // main shrinking loop
    //
    val foundFailure: State => quickcheck.Result => List[Rose[IO[quickcheck.Result]]] => IO[Unit] = st => res => ts => {
        localMin( st.copy(numTryShrinks = 0, isShrinking = True) )(res)(ts)
    }

    val localMin: State => quickcheck.Result => List[Rose[IO[quickcheck.Result]]] => IO[Unit] = st => res => {
        case Nil => for {
            _ <- Str.putLine(st.terminal)(
                res.reason ++:
                " (after " ++:
                Str.number(st.numSuccessTests+1)("test") ++:
                List.concat {
                    for {
                        _ <- List(())
                        if st.numSuccessShrinks > 0
                    } yield {
                        " and " ++: Str.number(st.numSuccessShrinks)("shrink")
                    }
                } ++:
                List.from("):  ") )
        } {
            callbackPostFinalFailure(st)(res)
        }
        case t :: ts => for {
            (res_, ts_) <- run(t)
            _ <- Str.putTemp(st.terminal)(
                Str.short(35)(res.reason) ++:
                " (after " ++: Str.number(st.numSuccessShrinks+1)("test") ++:
                List.concat {
                    for {
                        _ <- List(())
                        if (st.numSuccessShrinks > 0 || st.numTryShrinks > 0)
                    } yield {
                        " and " ++:
                        Show.show(st.numSuccessShrinks) ++:
                        List.concat {
                            for {
                                _ <- List(())
                                if st.numTryShrinks > 0
                            } yield {
                                "." ++: List.from(Show.show(st.numTryShrinks))
                            }
                        } ++:
                        " shrink" ++:
                        List.from( if (st.numSuccessShrinks == 1 && st.numTryShrinks == 0) "" else "s" )
                     }
                } ++:
                List.from(")...") )
            _ <- callbackPostTest(st)(res_)
        } {
            if (res_.ok == Just(False)) {
                foundFailure(st.copy(numSuccessShrinks = st.numSuccessShrinks + 1))(res_)(ts_)
            } else {
                localMin(st.copy(numTryShrinks = st.numTryShrinks + 1))(res)(ts)
            }
        }
    }

    // callbacks
    //
    val callbackPostTest: State => quickcheck.Result => IO[Unit] = st => res => {
        List.sequence__( for { PostTest(f) <- res.callbacks } yield f(st)(res) )
    }

    val callbackPostFinalFailure: State => quickcheck.Result => IO[Unit] = st => res => {
        List.sequence__( for { PostFinalFailure(f) <- res.callbacks } yield f(st)(res) )
    }
}
