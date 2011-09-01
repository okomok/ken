

// Copyright Shunsuke Sogame 2011.
//
// (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package parsec


trait Stream[s, m[+_], t] extends Monad[m] {
    type uncons = s => m[Maybe[(t, s)]]
    def uncons: uncons
}


object Stream extends StreamInstance {
    def apply[s, m <: Kind.Function1, t](implicit i: Stream[s, m#apply, t]): Stream[s, m#apply, t] = i
}


sealed trait StreamInstance { this: Stream.type =>
    implicit def ofList[tok, m[+_]](implicit i: Monad[m]): Stream[List[tok], m, tok] = new Stream[List[tok], m, tok] with MonadProxy[m] {
        private[this] type s = List[tok]
        private[this] type t = tok
        override val selfMonad = i
        override val uncons: uncons = {
            case Nil => `return`(Nothing)
            case t :: ts => `return`(Lazy(Just(t, ts)))
        }
    }
}
