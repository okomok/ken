

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


// type Reader[r, +a] = ReaderT[r, WeakIdentity.apply, a]


@Annotation.compilerWorkaround("2.9.1", 5031)
object _Reader extends ReaderTOp with Kind.FunctionLike {
    sealed trait apply1[r] extends Kind.Newtype1 {
        override type apply1[+a] = Reader[r, a]
        override type oldtype1[+a] = r => a
    }
    type apply[r] = apply1[r]

    def apply[r, a](n: r => a): Reader[r, a] = new ReaderT[r, WeakIdentity.apply, a](n)
    def unapply[r, a](m: Reader[r, a]): Option[r => a] = Some(m.run)
}
