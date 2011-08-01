

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


object ByName {

    implicit def fun2_2[a, b, c](f: a => b => c): a => (=> b) => c = { x => y => f(x)(y) }

}
