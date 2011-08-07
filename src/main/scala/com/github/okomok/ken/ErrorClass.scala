

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait ErrorClass[a] extends Typeclass0[a] {
    final val asErrorClass: ErrorClass[apply] = this

    // Core
    //
    def noMsg: a = strMsg("")
    def strMsg: String_ => a = { _ => noMsg }
}


trait ErrorClassProxy[a] extends ErrorClass[a] with Proxy {
    override def self: ErrorClass[a]

    override def noMsg: a = self.noMsg
    override def strMsg: String_ => a = self.strMsg
}


object ErrorClass extends ErrorClassInstance {
    def apply[a](implicit i: ErrorClass[a]): ErrorClass[a] = i
}


trait ErrorClassInstance { this: ErrorClass.type =>
    implicit val _ofString: ErrorClass[String_] = new ErrorClass[String_] {
        override def noMsg = ""
        override val strMsg = id[String_]_
    }

    implicit val _ofIOError: ErrorClass[IO.IOError] = new ErrorClass[IO.IOError] {
        override val strMsg = IO.userError
    }
}
