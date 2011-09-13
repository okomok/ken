

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2010 John Millikin
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package enumerator


private[enumerator] trait _Utilities[n[+_]] { this: _Enumerators[n] =>

    // Unsorted Utilities
    //
    def concatEnums[a, b](es: List[Enumerator[a, b]]): Enumerator[a, b] = List.foldl[Enumerator[a, b], Enumerator[a, b]](op_>==>:)(returnI)(es)

    def joinI[a, a_, b](outer: Iteratee[a, Step[a_, b]]): Iteratee[a, b] = {
        val mi = Monad[Iteratee.apply[a]]
        import mi.{>>=, `return`}
        lazy val check: Step[a_, b] => Iteratee[a, b] = {
            case Continue(k) => k(EOF) >>== {
                case Continue(_) => error("joinI: divergent iteratee")
                case s => check(s)
            }
            case Yield(x, _) => `return`(x)
            case Error(e) => throwError(e)
        }
        outer >>= check
    }

    def op_=@:[ao, ai, b](enum: Enumeratee[ao, ai, b])(iter: Iteratee[ai, b]): Iteratee[ao, b] = joinI(enum @@: iter)

    sealed class Op_=@:[ai, b](iter: Iteratee[ai, b]) {
        def =@:[ao](enum: Enumeratee[ao, ai, b]): Iteratee[ao, b] = op_=@:(enum)(iter)
    }
    implicit def =@:[ai, b](iter: Iteratee[ai, b]): Op_=@:[ai, b] = new Op_=@:(iter)

    def joinE[ao, ai, b](enum: Enumerator[ao, Step[ai, b]])(enee: Enumeratee[ao, ai, b]): Enumerator[ai, b] = s => Iteratee {
        for {
            step <- runIteratee(enumEOF[ao, Step[ai, b]] @@: enum @@: enee(s))
            * <- step match {
                case Error(err) => inner.`return`(Error(err))
                case Yield(x, _) => inner.`return`(x)
                case Continue(_) => error("joinE: divergent iteratee")
            }
        } yield *
    }

    def op_@=:[ao, ai, b](enum: Enumerator[ao, Step[ai, b]])(enee: Enumeratee[ao, ai, b]): Enumerator[ai, b] = joinE(enum)(enee)

    sealed class Op_@=:[ao, ai, b](enee: Enumeratee[ao, ai, b]) {
        def @=:(enum: Enumerator[ao, Step[ai, b]]): Enumerator[ai, b] = op_@=:(enum)(enee)
    }
    implicit def @=:[ao, ai, b](enee: Enumeratee[ao, ai, b]): Op_@=:[ao, ai, b] = new Op_@=:(enee)

    def sequence[ao, ai, b](i: Iteratee[ao, ai]): Enumeratee[ao, ai, b] = {
        val mi = Monad[Iteratee.apply[ao]]
        import mi.>>=
        lazy val loop: Enumeratee[ao, ai, b] = checkDone(check)
        lazy val check: (Stream[ai] => Iteratee[ai, b]) => Iteratee[ao, Step[ai, b]] = k => isEOF >>= { f =>
            if (f) `yield`(Continue(k))(EOF)
            else step(k)
        }
        lazy val step: (Stream[ai] => Iteratee[ai, b]) => Iteratee[ao, Step[ai, b]] = k => i >>= { v => k(Chunks(List(v))) >>== loop }
        loop
    }

    def enumEOF[a, b]: Enumerator[a, b] = {
        case Yield(x, _) => `yield`(x)(EOF)
        case Error(err) => throwError(err)
        case Continue(k) => {
            val check: Enumerator[a, b] = {
                case Continue(_) => error("enumEOF: divergent iteratee")
                case s => enumEOF(s)
            }
            k(EOF) >>== check
        }
    }

    def checkDoneEx[a, a_, b](extra: Stream[a])(f: (Stream[a] => Iteratee[a, b]) => Iteratee[a_, Step[a, b]]): Enumeratee[a_, a, b] = {
        case Continue(k) => f(k)
        case step => `yield`(step)(extra)
    }

    def checkDone[a, a_, b](f: (Stream[a] => Iteratee[a, b]) => Iteratee[a_, Step[a, b]]): Enumeratee[a_, a, b] = checkDoneEx(Chunks(Nil))(f)

    lazy val isEOF: Iteratee[Any, Bool] = continue {
        case s @ EOF => `yield`(True)(s)
        case s => `yield`(False)(s)
    }

    def tryIO[b](io: IO[b])(implicit i: MonadIO[n]): Iteratee[Any, b] = Iteratee {
        for {
            tried <- i.liftIO(SomeException.`try`(io))
        } yield tried match {
            case Right(b) => Yield(b, Chunks(Nil))
            case Left(err) => Error(err)
        }
    }

    def checkContinue0[a, b](inner: Enumerator[a, b] => (Stream[a] => Iteratee[a, b]) => Iteratee[a, b]): Enumerator[a, b] = {
        lazy val loop: Enumerator[a, b] = {
            case Continue(k) => inner(loop)(k)
            case step => returnI(step)
        }
        loop
    }

    def checkContinue1[a, b, s1](inner: (s1 => Enumerator[a, b]) => s1 => (Stream[a] => Iteratee[a, b]) => Iteratee[a, b])(s: s1): Enumerator[a, b] = {
        lazy val loop: s1 => Enumerator[a, b] = s => {
            case Continue(k) => inner(loop)(s)(k)
            case step => returnI(step)
        }
        loop(s)
    }

    // Utilities for testing and debugging
    //
    def printChunks(printEmpty: Bool)(implicit i: MonadIO[n]): Iteratee[Any, Unit] = {
        val mi = MonadIO[Iteratee.apply[Any]]
        import mi.`for`
        lazy val loop: Stream[Any] => Iteratee[Any, Unit] = {
            case Chunks(xs) => {
                val hide = List.`null`(xs) && Bool.not(printEmpty)
                for {
                    * <- mi.unless(hide)(mi.liftIO(IO.print(xs)))
                    * <- continue(loop)
                } yield *
            }
            case EOF => for {
                _ <- mi.liftIO(IO.putStrLn("EOF"))
                * <- `yield`()(EOF.of[Any])
            } yield *
        }
        continue(loop)
    }

    def enumList[a, b](n: Integer)(xs: List[a], * : Type[b] = null): Enumerator[a, b] = {
        lazy val loop: List[a] => Step[a, b] => Iteratee[a, b] = xs => {
            case Continue(k) if Bool.not(List.`null`(xs)) => {
                val (s1, s2) = List.genericSplitAt(n)(xs)
                k(Chunks(s1)) >>== loop(s2)
            }
            case step => returnI(step)
        }
        loop(xs)
    }
}
