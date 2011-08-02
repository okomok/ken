

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


object ByName {

    implicit def fromStrict[a, b, c](f: a => b => c): a => (=> b) => c = { x => y => f(x)(y) }
    implicit def toStrict[a, b, c](f: a => (=> b) => c): a => b => c = { x => y => f(x)(y) }

}
