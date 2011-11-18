

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


// type Cont[r, +a] = ContT[r, WeakIdentity.apply, a]


@Annotation.compilerWorkaround("2.9.1", 5031)
object _Cont extends ContTOp with Kind.FunctionLike {
    trait apply[r] extends apply1[r]
    trait apply1[r] extends ContT.apply2[r, WeakIdentity.apply]

    def apply[r, a](n: (a => r) => r): Cont[r, a] = new ContT[r, WeakIdentity.apply, a](n)
    def unapply[r, a](m: Cont[r, a]): Option[(a => r) => r] = Some(m.run)
}
