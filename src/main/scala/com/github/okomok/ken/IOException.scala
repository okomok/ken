

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


final case class IOException(rep: java.io.IOException)


object IOException extends Exception[IOException] with TypeableProxy[IOException] with ThisIsInstance {
    override val selfTypeable = Typeable.of[IOException]
}
