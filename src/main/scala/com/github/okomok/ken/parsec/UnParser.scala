

// Copyright Shunsuke Sogame 2011.
//
// (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package parsec


// Bridge between dependent types
//
trait UnParser[s, u, n[+_], +a] {
    def apply[b](v: UnParserParam[s, u, n, a, b]): n[b]
}


trait UnParserParam[s, u, n[+_], -a, +b] {
    type state = State[s, u]
    type cok = a => State[s, u] => ParseError => n[b]
    type cerr = ParseError => n[b]
    type eok = a => State[s, u] => ParseError => n[b]
    type eerr = ParseError => n[b]

    def state: state
    def cok: cok
    def cerr: cerr
    def eok: eok
    def eerr: eerr
}
