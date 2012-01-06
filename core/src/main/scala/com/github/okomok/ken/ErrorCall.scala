

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


final case class ErrorCall(s: String)


object ErrorCall extends Exception[ErrorCall] with TypeableProxy[ErrorCall] with ThisIsInstance {
    override val selfTypeable: selfTypeable = Typeable.of[ErrorCall]
}
