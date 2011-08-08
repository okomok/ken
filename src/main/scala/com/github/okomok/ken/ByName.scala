

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


// Rejected. Will be removed.

object ByName {

    implicit def fromStrict1[b, c](f: b => c): (=> b) => c = { y => f(y) }
    implicit def toStrict1[b, c](f: (=> b) => c): b => c = { y => f(y) }

    implicit def fromStrict2[a, b, c](f: a => b => c): a => (=> b) => c = { x => y => f(x)(y) }
    implicit def toStrict2[a, b, c](f: a => (=> b) => c): a => b => c = { x => y => f(x)(y) }

}
