

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


import scala.annotation.unchecked.uncheckedVariance // Why needed?


// Up is workaround: https://issues.scala-lang.org/browse/SI-4225
trait NewtypeOf[+a] extends Up[NewtypeOf[a]] with Kind.Newtype0 {
    override type apply0 = this.type
    override type oldtype0 = a @uncheckedVariance

    def get: a

    final def run: a = get
    final def app: a = get
/*
    Rejected.
    final def apply[b, c](b: b)(implicit ev: a <:< Function[b, c]): c = ev(get)(b)
    final def apply[b, c, d](b: b)(c: c)(implicit ev: a <:< Function[b, c => d]): d = ev(get)(b)(c)
    final def apply[b, c, d, e](b: b)(c: c)(d: d)(implicit ev: a <:< Function[b, c => d => e]): e = ev(get)(b)(c)(d)
*/
}


trait NewtypeOfProxy[+a] extends NewtypeOf[a] {
    def selfNewtype: NewtypeOf[a]

    override def get: a = selfNewtype.get
}


object NewtypeOf {
    def apply[a](a: a): NewtypeOf[a] = new NewtypeOf[a] {
        override def get: a = a
    }
}
