

// Copyright Shunsuke Sogame 2011.
//
// (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package parsec


final class Parsecs[s, u] extends ParsecsBase[s, u]

trait ParsecsBase[s, u] extends ParsecTsBase[s, u, WeakIdentity.type] {
    override val inner = Monad[WeakIdentity.type]

    type Parsec[+a] = ParsecT[a]
    final val Parsec = ParsecT

    def token[a, t](showToken: t => String_)
        (tokpos: t => SourcePos)
        (test: t => Maybe[a])(implicit i: Stream[s, WeakIdentity.apply, t]): Parsec[a] =
    {
        def nextpos(* : SourcePos)(tok: t)(ts: s): SourcePos = i.uncons(ts) match {
            case Nothing => tokpos(tok)
            case Just((tok_, _)) => tokpos(tok_)
        }
        tokenPrim(showToken)(nextpos)(test)
    }
}