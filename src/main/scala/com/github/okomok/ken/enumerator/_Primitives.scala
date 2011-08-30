

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2010 John Millikin
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package enumerator


private[ken] trait _Primitives[n[+_]] { this: _Enumerators[n] =>
    // Iteratee Operators
    //
    def op_>>==[a, b, a_, b_](i: Iteratee[a, b])(f: Step[a, b] => Iteratee[a_, b_]): Iteratee[a_, b_] = {
        import inner.>>=
        Iteratee { runIteratee(i) >>= (runIteratee[a_, b_]_ compose f) }
    }

    def op_==<<[a, b, a_, b_](f: Step[a, b] => Iteratee[a_, b_])(i: Iteratee[a, b]): Iteratee[a_, b_] = op_>>==(i)(f)
    def op_@@[a, b, a_, b_](f: Step[a, b] => Iteratee[a_, b_])(i: Iteratee[a, b]): Iteratee[a_, b_] = op_==<<(f)(i)

    def op_>==>[a, b, a_, b_](e1: Enumerator[a, b])(e2: Step[a, b] => Iteratee[a_, b_]): Step[a, b] => Iteratee[a_, b_] = s => e1(s) >>== e2
    def op_<==<[a, b, a_, b_](e2: Step[a, b] => Iteratee[a_, b_])(e1: Enumerator[a, b]): Step[a, b] => Iteratee[a_, b_] = op_>==>(e1)(e2)
/*
    sealed class Op_>>==[a, b](i: Iteratee[a, b]) {
        def >>==[a_, b_](f: Step[a, b] => Iteratee[a_, b_]): Iteratee[a_, b_] = op_>>==(i)(f)
    }
    implicit def >>==[a, b](i: Iteratee[a, b]): Op_>>==[a, b] = new Op_>>==(i)
*/
    sealed class Op_==<<[a, b, a_, b_](f: Step[a, b] => Iteratee[a_, b_]) {
        def ==<<(i: Iteratee[a, b]): Iteratee[a_, b_] = op_==<<(f)(i)
    }
    implicit def ==<<[a, b, a_, b_](f: Step[a, b] => Iteratee[a_, b_]): Op_==<<[a, b, a_, b_] = new Op_==<<(f)

    sealed class Op_@@[a, b, a_, b_](f: Step[a, b] => Iteratee[a_, b_]) {
        def @@(i: Iteratee[a, b]): Iteratee[a_, b_] = op_@@(f)(i)
    }
    implicit def @@[a, b, a_, b_](f: Step[a, b] => Iteratee[a_, b_]): Op_@@[a, b, a_, b_] = new Op_@@(f)

    sealed class Op_>==>[a, b](e1: Enumerator[a, b]) {
        def >==>[a_, b_](e2: Step[a, b] => Iteratee[a_, b_]): Step[a, b] => Iteratee[a_, b_] = op_>==>(e1)(e2)
    }
    implicit def >==>[a, b](e1: Enumerator[a, b]): Op_>==>[a, b] = new Op_>==>(e1)

    sealed class Op_<==<[a, b, a_, b_](e2: Step[a, b] => Iteratee[a_, b_]) {
        def <==<(e1: Enumerator[a, b]): Step[a, b] => Iteratee[a_, b_] = op_<==<(e2)(e1)
    }
    implicit def <==<[a, b, a_, b_](e2: Step[a, b] => Iteratee[a_, b_]): Op_<==<[a, b, a_, b_] = new Op_<==<(e2)

    // Primitives
    //
    def run[a, b](i: Iteratee[a, b]): n[Either[Throwable, b]] = {
        import inner.{forComp, `return`}
        for {
            mStep <- runIteratee { enumEOF[a, b] ==<< i }
            * <- mStep match {
                case Error(err) => `return` { Left(err) }
                case Yield(x, _) => `return`{ Right(x) }
                case Continue(_) => error("run: divergent iteratee")
            }
        } yield *
    }

    def run_[b](i: Iteratee[_, b]): n[b] = {
        import inner.>>=
        run(i) >>= Either.either((x: Throwable) => throw x)(inner.`return`[b])
    }

    def throwError[a, b](exc: Throwable): Iteratee[a, b] = returnI(Error(exc))

    def catchError[a, b](iter: Iteratee[a, b])(h: Throwable => Iteratee[a, b]): Iteratee[a, b] = {
        def step(s: Step[a, b]): Iteratee[a, b] = s match {
            case Yield(b, as) => `yield`(b)(as)
            case Error(err) => h(err)
            case Continue(k) => continue(s => k(s) >>== step)
        }
        iter >>== step
    }
}
