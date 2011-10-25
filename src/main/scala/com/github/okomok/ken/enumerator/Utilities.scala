

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2010 John Millikin
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package enumerator


private[enumerator] trait Utilities { this: _Enumerator.type =>

    /**
     * Appends all the enumerators of `es`.
     */
    def concatEnums[a, n[+_], b](es: List[Enumerator[a, n, b]])(implicit im: Monad[n]): Enumerator[a, n, b] = List.foldl[Enumerator[a, n, b], Enumerator[a, n, b]](op_>==>:)(returnI)(es)

    def joinI[a, n[+_], a_, b](outer: Iteratee[a, n, Step[a_, n, b]])(implicit im: Monad[n]): Iteratee[a, n, b] = {
        val m = Monad[Iteratee.apply2[a, n]]
        import m.{>>=, `return`}
        lazy val check: Step[a_, n, b] => Iteratee[a, n, b] = {
            case Continue(k) => k(EOF) >>== {
                case Continue(_) => error("joinI: divergent iteratee")
                case s => check(s)
            }
            case Yield(x, _) => `return`(x)
            case Error(e) => throwError(e)
        }
        outer >>= check
    }

    def op_=@:[ao, ai, n[+_], b](enum: Enumeratee[ao, ai, n, b])(iter: Iteratee[ai, n, b])(implicit im: Monad[n]): Iteratee[ao, n, b] = joinI(enum @@: iter)

    private[ken] sealed class Op_=@:[ai, n[+_], b](iter: Iteratee[ai, n, b]) {
        def =@:[ao](enum: Enumeratee[ao, ai, n, b])(implicit im: Monad[n]): Iteratee[ao, n, b] = op_=@:(enum)(iter)
    }
    implicit def =@:[ai, n[+_], b](iter: Iteratee[ai, n, b]): Op_=@:[ai, n, b] = new Op_=@:(iter)

    def joinE[ao, ai, n[+_], b](enum: Enumerator[ao, n, Step[ai, n, b]])(enee: Enumeratee[ao, ai, n, b])(implicit im: Monad[n]): Enumerator[ai, n, b] = s => Iteratee {
        import im.`for`
        for {
            step <- runIteratee(enumEOF[ao, n, Step[ai, n, b]] @@: enum @@: enee(s))
        } {
            step match {
                case Error(err) => im.`return`(Error(err))
                case Yield(x, _) => im.`return`(x)
                case Continue(_) => error("joinE: divergent iteratee")
            }
        }
    }

    def op_@=:[ao, ai, n[+_], b](enum: Enumerator[ao, n, Step[ai, n, b]])(enee: Enumeratee[ao, ai, n, b])(implicit im: Monad[n]): Enumerator[ai, n, b] = joinE(enum)(enee)

    private[ken] sealed class Op_@=:[ao, ai, n[+_], b](enee: Enumeratee[ao, ai, n, b]) {
        def @=:(enum: Enumerator[ao, n, Step[ai, n, b]])(implicit im: Monad[n]): Enumerator[ai, n, b] = op_@=:(enum)(enee)
    }
    implicit def @=:[ao, ai, n[+_], b](enee: Enumeratee[ao, ai, n, b]): Op_@=:[ao, ai, n, b] = new Op_@=:(enee)

    def sequence[ao, ai, n[+_], b](i: Iteratee[ao, n, ai])(implicit im: Monad[n]): Enumeratee[ao, ai, n, b] = {
        val mi = Monad[Iteratee.apply2[ao, n]]
        import mi.>>=
        lazy val loop: Enumeratee[ao, ai, n, b] = checkDone(check)
        lazy val check: (Stream[ai] => Iteratee[ai, n, b]) => Iteratee[ao, n, Step[ai, n, b]] = k => isEOF >>= { f =>
            if (f) `yield`(Continue(k))(EOF)
            else step(k)
        }
        lazy val step: (Stream[ai] => Iteratee[ai, n, b]) => Iteratee[ao, n, Step[ai, n, b]] = k => i >>= { v => k(Chunks(List(v))) >>== loop }
        loop
    }

    /**
     * An empty enumerator
     */
    def enumEOF[a, n[+_], b](implicit im: Monad[n]): Enumerator[a, n, b] = {
        case Yield(x, _) => `yield`(x)(EOF)
        case Error(err) => throwError(err)
        case Continue(k) => {
            val check: Enumerator[a, n, b] = {
                case Continue(_) => error("enumEOF: divergent iteratee")
                case s => enumEOF(im)(s)
            }
            k(EOF) >>== check
        }
    }

    def checkDoneEx[a, n[+_], b, a_](extra: Stream[a])(f: (Stream[a] => Iteratee[a, n, b]) => Iteratee[a_, n, Step[a, n, b]])(implicit im: Monad[n]): Enumeratee[a_, a, n, b] = {
        case Continue(k) => f(k)
        case step => `yield`(step)(extra)
    }

    def checkDone[a, n[+_], b, a_](f: (Stream[a] => Iteratee[a, n, b]) => Iteratee[a_, n, Step[a, n, b]])(implicit im: Monad[n]): Enumeratee[a_, a, n, b] = checkDoneEx(Chunks(Nil))(f)

    def isEOF[n[+_]](implicit im: Monad[n]): Iteratee[Any, n, Bool] = continue {
        case s @ EOF => `yield`(True)(s)
        case s => `yield`(False)(s)
    }

    def tryIO[n[+_], b](io: IO[b])(implicit im: MonadIO[n]): Iteratee[Any, n, b] = Iteratee {
        import im.`for`
        for {
            tried <- im.liftIO(SomeException.`try`(io))
        } yield tried match {
            case Right(b) => Yield(b, Chunks(Nil))
            case Left(err) => Error(err)
        }
    }

    def checkContinue0[a, n[+_], b](inner: Enumerator[a, n, b] => (Stream[a] => Iteratee[a, n, b]) => Iteratee[a, n, b])(implicit im: Monad[n]): Enumerator[a, n, b] = {
        lazy val loop: Enumerator[a, n, b] = {
            case Continue(k) => inner(loop)(k)
            case step => returnI(step)
        }
        loop
    }

    def checkContinue1[a, n[+_], b, s1](inner: (s1 => Enumerator[a, n, b]) => s1 => (Stream[a] => Iteratee[a, n, b]) => Iteratee[a, n, b])(s: s1)(implicit im: Monad[n]): Enumerator[a, n, b] = {
        lazy val loop: s1 => Enumerator[a, n, b] = s => {
            case Continue(k) => inner(loop)(s)(k)
            case step => returnI(step)
        }
        loop(s)
    }

    // Utilities for testing and debugging
    //
    def printChunks[n[+_]](printEmpty: Bool)(implicit i: MonadIO[n]): Iteratee[Any, n, Unit] = {
        val mi = MonadIO[Iteratee.apply2[Any, n]]
        import mi.`for`
        implicit val as = Show.of[Any]
        lazy val loop: Stream[Any] => Iteratee[Any, n, Unit] = {
            case Chunks(xs) => {
                val hide = List.`null`(xs) && Bool.not(printEmpty)
                for {
                    _ <- mi.unless(hide)(mi.liftIO(IO.print(xs)))
                } {
                    continue(loop)
                }
            }
            case EOF => for {
                _ <- mi.liftIO(IO.putStrLn("EOF"))
            } {
                `yield`()(EOF.of[Any])
            }
        }
        continue(loop)
    }

    /**
     * Builds an enumerator whose elements are those of a list.
     */
    def enumList[a, n[+_], b](n: Integer)(xs: List[a], * : Type[b] = null)(implicit im: Monad[n]): Enumerator[a, n, b] = {
        lazy val loop: List[a] => Step[a, n, b] => Iteratee[a, n, b] = xs => {
            case Continue(k) if Bool.not(List.`null`(xs)) => {
                val (s1, s2) = List.genericSplitAt(n)(xs)
                k(Chunks(s1)) >>== loop(s2)
            }
            case step => returnI(step)
        }
        loop(xs)
    }
}
