

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


// Up is workaround: https://issues.scala-lang.org/browse/SI-4225
trait NewtypeOf[+a] extends Up[NewtypeOf[a]] {
    def get: a
    final def run: a = get
    final def app: a = get
}


trait NewtypeOfProxy[+a] extends NewtypeOf[a] with Proxy {
    override def self: NewtypeOf[a]
    override def get: a = self.run
}


object NewtypeOf {
    def apply[a](a: a): NewtypeOf[a] = new NewtypeOf[a] {
        override def get: a = a
    }
}
