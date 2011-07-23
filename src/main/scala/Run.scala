

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Run[+a] {
    def run: a
}

object Run {
    def apply[a](a: a): Run[a] = new Run[a] {
        override def run: a = a
    }
}
