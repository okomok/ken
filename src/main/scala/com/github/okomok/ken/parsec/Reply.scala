

// Copyright Shunsuke Sogame 2011.
//
// (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package parsec


sealed abstract class Reply[+s, +u, +a] extends Up[Reply[s, u, a]]

final case class Ok[+s, +u, +a](x: a, state: State[s, u], err: ParseError) extends Reply[s, u, a]
final case class Error(err: ParseError) extends Reply[Nothing, Nothing, Nothing]


object Reply extends Kind.FunctionLike {
    sealed trait apply[s, u] extends Kind.Function1 {
        override type apply1[+a] = Reply[s, u, a]
    }

    implicit def _asFunctor[s, u]: Functor[({type f[+a] = Reply[s, u, a]})#f] = new Functor[({type f[+a] = Reply[s, u, a]})#f] {
        private type f[+a] = Reply[s, u, a]
        override def fmap[a, b](f: a => b)(x: f[a]): f[b] = x match {
            case Ok(x, s, e) => Ok(f(x), s, e)
            case Error(e) => Error(e)
        }
    }
}
