

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


object Pair {
    sealed trait apply[a] extends Kind.Function1 {
        override type apply[+b] = Pair[a, b]
    }
}
