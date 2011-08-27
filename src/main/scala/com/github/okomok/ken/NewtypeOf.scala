

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


// Up is workaround: https://issues.scala-lang.org/browse/SI-4225
trait NewtypeOf[+a] extends Up[NewtypeOf[a]] with Kind.constThis {
    def get: a

    final def run: a = get
    final def app: a = get

    final def apply[b, c](x: b)(implicit ev: a <:< Function1[b, c]): c = ev(get)(x)
}


trait NewtypeOfProxy[+a] extends NewtypeOf[a] {
    def selfNewtypeOf: NewtypeOf[a]

    override def get: a = selfNewtypeOf.run
}


object NewtypeOf {
    def apply[a](a: a): NewtypeOf[a] = new NewtypeOf[a] {
        override def get: a = a
    }
}
