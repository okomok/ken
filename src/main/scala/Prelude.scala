

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


object Prelude {

    def id[a](x: a): a = x

    def const[a, b](x: a)(y: b): a = x

    def apply[a, b](x: a => b)(y: a): b = x(y)

    def flip[a, b, c](x: a => b => c)(y: b)(z: a): c = x(z)(y)

    /*
    sealed class ForwardPipe[A](x: A) {
        def |>[B](f: A => B): B = f(x)
    }
    implicit def |>[A](x: A): ForwardPipe[A] = new ForwardPipe(x)

    sealed class BackwardPipe[A, B](f: A => B) {
        def <|(x: A): B = f(x)
    }
    implicit def <|[A, B](f: A => B): BackwardPipe[A, B] = new BackwardPipe(f)

    sealed class SideEffectPipe[A](x: A) {
        def |<(f: A => Any): A = { f(x); x }
    }
    implicit def |<[A](x: A): SideEffectPipe[A] = new SideEffectPipe(x)
    */

}
