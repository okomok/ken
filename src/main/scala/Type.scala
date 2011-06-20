

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


sealed abstract class Type[a] {
    type get = a
}

object Type {
    def apply[a]: Type[a] = new Type[a] {}
}