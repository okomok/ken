

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait ArrowLoop[a[_, _]] extends Arrow[a] {
    def loop[b, c, d](f: a[(b, d), (c, d)]): a[b, c]
}


object ArrowLoop {
    def apply[a[_, _]](implicit i: ArrowLoop[a]): ArrowLoop[a] = i
}

