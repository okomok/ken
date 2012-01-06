

// Copyright Shunsuke Sogame 2011.
//
// (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package parsec


object ParsecTOp {
    def apply[p <: KindParsecT](implicit i: Monad[p#innerMonad]): ParsecTOp[p#stream, p#userState, p#innerMonad] = new ParsecTOp[p#stream, p#userState, p#innerMonad] {
        override val innerMonad: Monad[p#innerMonad] = i
    }
}


sealed trait ParsecTOp[s, u, n[+_]] extends Prim[s, u, n] with Combinators[s, u, n] with Char_[s, u, n] {
    protected implicit val innerMonad: Monad[n]
    protected implicit final lazy val parsecMonad: MonadPlus[({type L[+a] = ParsecT[s, u, n, a]})#L] = ParsecT._asMonadPlus[s, u, n]

    protected implicit final def innerFor[a](x: n[a]): innerMonad.For[a] = innerMonad.`for`(x)
    protected implicit final def innerOp_>>=[a](x: n[a]): innerMonad.Op_>>=[a] = innerMonad.>>=(x)
}
