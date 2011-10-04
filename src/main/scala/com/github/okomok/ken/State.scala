

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


@Annotation.compilerWorkaround("2.9.1", 5031) // `State` instead of `State`_ crashes scalac in user-site.
object _State extends StateTOp with Kind.FunctionLike {
    sealed trait apply1[s] extends Kind.Function1 {
        override type apply1[+a] = State[s, a]
    }
    type apply[s] = apply1[s]

    def apply[s, a](z: s => (a, s)): State[s, a] = new StateT[s, WeakIdentity.apply, a](z)
    def unapply[s, a](x: State[s, a]): Option[s => (a, s)] = Some(x.get)
}
