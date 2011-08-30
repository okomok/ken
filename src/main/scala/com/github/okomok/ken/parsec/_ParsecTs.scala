

// Copyright Shunsuke Sogame 2011.
//
// (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package parsec


private[ken] final class _ParsecTs[n[+_]](val inner: Monad[n]) {

    // ParsecT
    //
    trait _ParsecT[s, u, +a] extends Kind.constThis {
        def apply[b](s: State[s, u])
            (cok: a => State[s, u] => ParseError => n[b])
            (cerr: a => ParseError => n[b])
            (eok: a => State[s, u] => ParseError => n[b])
            (eerr: ParseError => m[b]): n[b]

        def run(s: State[s, u]): n[Consumed[n[Reply[s, u, a]]]]

    }

    object _ParsecT {
    }
}
