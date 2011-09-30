

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


final case class ErrorCall(s: String)


object ErrorCall extends Eq.Default[ErrorCall] with Show.Default[ErrorCall]
    with Exception[ErrorCall] with TypeableProxy[ErrorCall] with ThisIsInstance
{
    override val selfTypeable = Typeable.of[ErrorCall]
}
