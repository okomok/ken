

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


// type State[s, +a] = StateT[s, WeakIdentity.apply, a]


// @scalacWorkaround("2.9.1", 5031)
object _State extends StateTOp with Kind.FunctionLike {
    trait apply[s] extends apply1[s]
    trait apply1[s] extends StateT.apply2[s, WeakIdentity.type]

    def apply[s, a](n: s => (a, s)): State[s, a] = new StateT[s, WeakIdentity.apply, a](n)
    def unapply[s, a](m: State[s, a]): Option[s => (a, s)] = Some(m.run)
}
