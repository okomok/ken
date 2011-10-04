

// Copyright Shunsuke Sogame 2011.
//
// Copyright 2004, The University Court of the University of Glasgow.
// All rights reserved.
//
// Copyright (c) 2002 Simon Peyton Jones
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


// type Error[e, +a] = ErrorT[e, WeakIdentity.apply, a]


@Annotation.compilerWorkaround("2.9.1", 5031)
object _Error extends ErrorTOp with Kind.FunctionLike {
    sealed trait apply1[e] extends Kind.Newtype1 {
        override type apply1[+a] = Error[e, a]
        override type oldtype1[+a] = Either[e, a]
    }
    type apply[e] = apply1[e]

    def apply[e, a](n: Either[e, a]): Error[e, a] = new ErrorT[e, WeakIdentity.apply, a](n)
    def unapply[e, a](m: Error[e, a]): Option[Either[e, a]] = Some(m.run)
}
