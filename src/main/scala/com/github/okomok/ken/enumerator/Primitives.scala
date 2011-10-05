

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2010 John Millikin
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package enumerator


private[enumerator] trait Primitives { this: _Enumerator.type =>

    /**
     * Pre-subscription before `run`. `run(i >>== f)` means `f.foreach(i)`
     */
    def op_>>==[a, n[+_], b, a_, b_](i: Iteratee[a, n, b])(f: Step[a, n, b] => Iteratee[a_, n, b_])(implicit im: Monad[n]): Iteratee[a_, n, b_] = {
        import im.>>=
        Iteratee { runIteratee(i) >>= (runIteratee[a_, n, b_]_ `.` f) }
    }

    @Annotation.flipOf("op_>>==")
    def op_==<<:[a, n[+_], b, a_, b_](f: Step[a, n, b] => Iteratee[a_, n, b_])(i: Iteratee[a, n, b])(implicit im: Monad[n]): Iteratee[a_, n, b_] = op_>>==(i)(f)

    @Annotation.aliasOf("op_==<<:")
    def op_@@:[a, b, n[+_], a_, b_](f: Step[a, n, b] => Iteratee[a_, n, b_])(i: Iteratee[a, n, b])(implicit im: Monad[n]): Iteratee[a_, n, b_] = op_==<<:(f)(i)

    /**
     * Appends two enumerators.
     */
    def op_>==>:[a, b, n[+_], a_, b_](e1: Enumerator[a, n, b])(e2: Step[a, n, b] => Iteratee[a_, n, b_])(implicit im: Monad[n]): Step[a, n, b] => Iteratee[a_, n, b_] = s => e1(s) >>== e2

    @Annotation.flipOf("op_>==>:")
    def op_<==<:[a, b, n[+_], a_, b_](e2: Step[a, n, b] => Iteratee[a_, n, b_])(e1: Enumerator[a, n, b])(implicit im: Monad[n]): Step[a, n, b] => Iteratee[a_, n, b_] = op_>==>:(e1)(e2)

    private[ken] sealed class Op_>>==[a, n[+_], b](i: Iteratee[a, n, b]) {
        def >>==[a_, b_](f: Step[a, n, b] => Iteratee[a_, n, b_])(implicit im: Monad[n]): Iteratee[a_, n, b_] = op_>>==(i)(f)
    }
    implicit def >>==[a, n[+_], b](i: Iteratee[a, n, b]): Op_>>==[a, n, b] = new Op_>>==(i)

    private[ken] sealed class Op_==<<:[a, n[+_], b](i: Iteratee[a, n, b]) {
        def ==<<:[a_, b_](f: Step[a, n, b] => Iteratee[a_, n, b_])(implicit im: Monad[n]): Iteratee[a_, n, b_] = op_==<<:(f)(i)
    }
    implicit def ==<<:[a, n[+_], b](i: Iteratee[a, n, b]): Op_==<<:[a, n, b] = new Op_==<<:(i)

    private[ken] sealed class Op_@@:[a, n[+_], b](i: Iteratee[a, n, b]) {
        def @@:[a_, b_](f: Step[a, n, b] => Iteratee[a_, n, b_])(implicit im: Monad[n]): Iteratee[a_, n, b_] = op_@@:(f)(i)
    }
    implicit def @@:[a, n[+_], b](i: Iteratee[a, n, b]): Op_@@:[a, n, b] = new Op_@@:(i)

    private[ken] sealed class Op_>==>:[a, n[+_], b, a_, b_](e2: Step[a, n, b] => Iteratee[a_, n, b_]) {
        def >==>:(e1: Enumerator[a, n, b])(implicit im: Monad[n]): Step[a, n, b] => Iteratee[a_, n, b_] = op_>==>:(e1)(e2)
    }
    implicit def >==>:[a, b, n[+_], a_, b_](e2: Step[a, n, b] => Iteratee[a_, n, b_]): Op_>==>:[a, n, b, a_, b_] = new Op_>==>:(e2)

    private[ken] sealed class Op_<==<:[a, n[+_], b](e1: Enumerator[a, n, b]) {
        def <==<:[a_, b_](e2: Step[a, n, b] => Iteratee[a_, n, b_])(implicit im: Monad[n]): Step[a, n, b] => Iteratee[a_, n, b_] = op_<==<:(e2)(e1)
    }
    implicit def <==<:[a, n[+_], b](e1: Enumerator[a, n, b]): Op_<==<:[a, n, b] = new Op_<==<:(e1)

    /**
     * Sends the EOF to an iteratee, then retrieves a result.
     */
    def run[a, n[+_], b](i: Iteratee[a, n, b])(implicit im: Monad[n]): n[Either[SomeException, b]] = {
        import im.{`return`, `for`}
        for {
            mStep <- runIteratee { enumEOF[a, n, b] ==<<: i }
            * <- mStep match {
                case Error(err) => `return` { Left(err) }
                case Yield(x, _) => `return` { Right(x) }
                case Continue(_) => error("run: divergent iteratee")
            }
        } yield *
    }

    /**
     * Same as `run` but throws in case of an error.
     */
    def run_[n[+_], b](i: Iteratee[_, n, b])(implicit im: Monad[n]): n[b] = {
        import im.>>=
        run(i) >>= Either.either((x: Throwable) => throw x)(im.`return`[b])
    }

    /**
     * Iteratee which always informs an error.
     */
    def throwError[n[+_], e](exc: e)(implicit i: Exception[e], im: Monad[n]): Iteratee[Any, n, Nothing] = returnI(Error(i.toException(exc)))

    def catchError[a, n[+_], b](iter: Iteratee[a, n, b])(h: SomeException => Iteratee[a, n, b])(implicit im: Monad[n]): Iteratee[a, n, b] = {
        lazy val step: Step[a, n, b] => Iteratee[a, n, b] = {
            case Yield(b, as) => `yield`(b)(as)
            case Error(err) => h(err)
            case Continue(k) => continue(s => k(s) >>== step)
        }
        iter >>== step
    }
}
