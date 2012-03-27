

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


// @scalacWorkaround("2.9.1", 5031)
object _Writer extends WriterTOp with Kind.FunctionLike {
    trait apply[w] extends apply1[w]
    trait apply1[w] extends WriterT.apply2[w, WeakIdentity.type]

    def apply[w, a](n: (a, w)): Writer[w, a] = new WriterT[w, WeakIdentity.apply, a](n)
    def unapply[w, a](m: Writer[w, a]): Option[(a, w)] = Some(m.run)
}
