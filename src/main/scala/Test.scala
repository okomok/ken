

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


object Test {
    val i = Applicative.ofFunction1[Int]
    import i.<@>

    val r: Int => Int => Int = ((x: Int) => (y: Int) => x + y) <@> ((x: Int) => x)
}
