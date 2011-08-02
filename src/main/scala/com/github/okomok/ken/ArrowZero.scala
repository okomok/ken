

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


// TODO


trait ArrowZero[a[_, _]] extends Arrow[a] {
    def zeroArrow[b, c]: a[b, c]
}


object ArrowZero {
    def apply[a[_, _]](implicit i: ArrowZero[a]): ArrowZero[a] = i
}
