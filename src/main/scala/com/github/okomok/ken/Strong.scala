

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


/**
 * Same as Scalaz NewType.
 */
trait Strong[+a] {
    def run: a
}


trait StrongProxy[+a] extends Strong[a] with Proxy {
    override def self: Strong[a]
    override def run: a = self.run
}
