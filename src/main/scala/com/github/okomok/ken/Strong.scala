

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


// Up is workaround: https://issues.scala-lang.org/browse/SI-4225
trait Strong[+a] extends Up[Strong[a]] {
    def get: a

    final def run: a = get
    final def app: a = get

    final def apply[b, c](x: b)(implicit ev: a <:< Function1[b, c]): c = ev(get)(x)
}


trait StrongProxy[+a] extends Strong[a] {
    def selfStrong: Strong[a]

    override def get: a = selfStrong.run
}


object Strong {
    def apply[a](a: a): Strong[a] = new Strong[a] {
        override def get: a = a
    }
}
