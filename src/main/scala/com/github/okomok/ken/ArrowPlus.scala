

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait ArrowPlus[a[_, _]] extends ArrowZero[a] {
    def op_<+>[b, c](f: a[b, c])(g: => a[b, c]): a[b, c]

    final private[ken] class Op_<+>[b, c](f: a[b, c]) {
        def <+>(g: => a[b, c]): a[b, c] = op_<+>(f)(g)
    }
    final implicit def <+>[b, c](f: a[b, c]): Op_<+>[b, c] = new Op_<+>[b, c](f)
}


object ArrowPlus {
    def apply[a[_, _]](implicit i: ArrowPlus[a]): ArrowPlus[a] = i
}
