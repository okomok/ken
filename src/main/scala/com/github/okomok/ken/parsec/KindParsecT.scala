

// Copyright Shunsuke Sogame 2011.
//
// (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package parsec


trait KindParsecT extends Kind.Newtype1 {
    type stream
    type userState
    type innerMonad[+a]
}
