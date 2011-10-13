

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


import scala.annotation.unchecked.uncheckedVariance // Why needed?


trait NewtypeOf[+a] extends Up[NewtypeOf[a]] with Kind.Newtype0 {
    override type apply0 = this.type
    override type oldtype0 = a @uncheckedVariance

    def old: a

    final def get: a = old
    final def run: a = old
    final def app: a = old
}


trait NewtypeOfProxy[+a] extends NewtypeOf[a] {
    def selfNewtypeOf: NewtypeOf[a]

    override def old: a = selfNewtypeOf.old
}
