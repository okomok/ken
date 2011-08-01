

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait ArrowApply[a[_, _]] extends Arrow[a] {
    def app[b, c]: a[(a[b, c], b), c]
}


object ArrowApply {
    def apply[a[_, _]](implicit i: ArrowApply[a]): ArrowApply[a] = i
}

