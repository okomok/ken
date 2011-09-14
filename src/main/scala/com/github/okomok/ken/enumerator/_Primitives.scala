

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2010 John Millikin
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package enumerator


private[enumerator] trait _Primitives[n[+_]] { this: _Enumerators[n] =>

    /**
     * Pre-subscription before `run`. `run(i >>== f)` means `f.foreach(i)`
     */
    def op_>>==[a, b, a_, b_](i: Iteratee[a, b])(f: Step[a, b] => Iteratee[a_, b_]): Iteratee[a_, b_] = {
        Iteratee { runIteratee(i) >>= (runIteratee[a_, b_]_ `.` f) }
    }

    @Annotation.flipOf("op_>>==")
    def op_==<<:[a, b, a_, b_](f: Step[a, b] => Iteratee[a_, b_])(i: Iteratee[a, b]): Iteratee[a_, b_] = op_>>==(i)(f)

    @Annotation.aliasOf("op_==<<:")
    def op_@@:[a, b, a_, b_](f: Step[a, b] => Iteratee[a_, b_])(i: Iteratee[a, b]): Iteratee[a_, b_] = op_==<<:(f)(i)

    /**
     * Appends two enumerators.
     */
    def op_>==>:[a, b, a_, b_](e1: Enumerator[a, b])(e2: Step[a, b] => Iteratee[a_, b_]): Step[a, b] => Iteratee[a_, b_] = s => e1(s) >>== e2

    @Annotation.flipOf("op_>==>:")
    def op_<==<:[a, b, a_, b_](e2: Step[a, b] => Iteratee[a_, b_])(e1: Enumerator[a, b]): Step[a, b] => Iteratee[a_, b_] = op_>==>:(e1)(e2)

    sealed class Op_>>==[a, b](i: Iteratee[a, b]) {
        def >>==[a_, b_](f: Step[a, b] => Iteratee[a_, b_]): Iteratee[a_, b_] = op_>>==(i)(f)
    }
    implicit def >>==[a, b](i: Iteratee[a, b]): Op_>>==[a, b] = new Op_>>==(i)

    sealed class Op_==<<:[a, b](i: Iteratee[a, b]) {
        def ==<<:[a_, b_](f: Step[a, b] => Iteratee[a_, b_]): Iteratee[a_, b_] = op_==<<:(f)(i)
    }
    implicit def ==<<:[a, b](i: Iteratee[a, b]): Op_==<<:[a, b] = new Op_==<<:(i)

    sealed class Op_@@:[a, b](i: Iteratee[a, b]) {
        def @@:[a_, b_](f: Step[a, b] => Iteratee[a_, b_]): Iteratee[a_, b_] = op_@@:(f)(i)
    }
    implicit def @@:[a, b](i: Iteratee[a, b]): Op_@@:[a, b] = new Op_@@:(i)

    sealed class Op_>==>:[a, b, a_, b_](e2: Step[a, b] => Iteratee[a_, b_]) {
        def >==>:(e1: Enumerator[a, b]): Step[a, b] => Iteratee[a_, b_] = op_>==>:(e1)(e2)
    }
    implicit def >==>:[a, b, a_, b_](e2: Step[a, b] => Iteratee[a_, b_]): Op_>==>:[a, b, a_, b_] = new Op_>==>:(e2)

    sealed class Op_<==<:[a, b](e1: Enumerator[a, b]) {
        def <==<:[a_, b_](e2: Step[a, b] => Iteratee[a_, b_]): Step[a, b] => Iteratee[a_, b_] = op_<==<:(e2)(e1)
    }
    implicit def <==<:[a, b](e1: Enumerator[a, b]): Op_<==<:[a, b] = new Op_<==<:(e1)

    /**
     * Sends the EOF to an iteratee, then retrieves a result.
     */
    def run[a, b](i: Iteratee[a, b]): n[Either[SomeException, b]] = {
        import inner.`return`
        for {
            mStep <- runIteratee { enumEOF[a, b] ==<<: i }
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
    def run_[b](i: Iteratee[_, b]): n[b] = {
        run(i) >>= Either.either((x: Throwable) => throw x)(inner.`return`[b])
    }

    /**
     * Iteratee which always informs an error.
     */
    def throwError[e](exc: e)(implicit i: Exception[e]): Iteratee[Any, Nothing] = returnI(Error(i.toException(exc)))

    def catchError[a, b](iter: Iteratee[a, b])(h: SomeException => Iteratee[a, b]): Iteratee[a, b] = {
        lazy val step: Step[a, b] => Iteratee[a, b] = {
            case Yield(b, as) => `yield`(b)(as)
            case Error(err) => h(err)
            case Continue(k) => continue(s => k(s) >>== step)
        }
        iter >>== step
    }
}
