

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait ArrowPlus[a[_, _]] extends ArrowZero[a] {
    def op_<+>[b, c](f: a[b, c])(g: => a[b, c]): a[b, c]
}
