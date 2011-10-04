

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


// type Writer[w, +a] = WriterT[w, WeakIdentity.apply, a]


@Annotation.compilerWorkaround("2.9.1", 5031)
object _Writer extends WriterTOp with Kind.FunctionLike {
    sealed trait apply1[w] extends Kind.Newtype1 {
        override type apply1[+a] = Writer[w, a]
        override type oldtype1[+a] = (a, w)
    }
    type apply[w] = apply1[w]

    def apply[w, a](n: (a, w)): Writer[w, a] = new WriterT[w, WeakIdentity.apply, a](n)
    def unapply[w, a](m: Writer[w, a]): Option[(a, w)] = Some(m.run)
}
