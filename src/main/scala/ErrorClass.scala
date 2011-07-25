

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait ErrorClass[+a] extends Klass {
    type apply = a

    final def asErrorClass: ErrorClass[a] = this

    def noMsg: a = strMsg("")
    def strMsg: String_ => a = { _ => noMsg }
}


trait ErrorClassProxy[+a] extends ErrorClass[a] with Proxy {
    override def self: ErrorClass[a]
    override def noMsg: a = self.noMsg
    override def strMsg: String_ => a = self.strMsg
}


object ErrorClass {
    implicit val ofString: ErrorClass[String_] = new ErrorClass[String_] {
        override def noMsg = ""
        override val strMsg = id[String_]_
    }

    implicit val ofIOError: ErrorClass[IO.IOError] = new ErrorClass[IO.IOError] {
        override val strMsg = IO.userError
    }
}
