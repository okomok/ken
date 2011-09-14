

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


// Up is workaround: https://issues.scala-lang.org/browse/SI-4225
trait Strong[+a] extends Up[Strong[a]] {
    def get: a

    final def run: a = get
    final def app: a = get
/*
    final def apply[b, c](b: b)(implicit ev: a <:< Function[b, c]): c = ev(get)(b)
    final def apply[b, c, d](b: b)(c: c)(implicit ev: a <:< Function[b, c => d]): d = ev(get)(b)(c)
    final def apply[b, c, d, e](b: b)(c: c)(d: d)(implicit ev: a <:< Function[b, c => d => e]): e = ev(get)(b)(c)(d)
*/
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
