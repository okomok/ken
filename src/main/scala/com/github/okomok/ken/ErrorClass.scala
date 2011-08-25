

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait ErrorClass[a] extends Typeclass0[a] {
    final val asErrorClass: ErrorClass[apply0] = this

    // Core
    //
    def noMsg: a = strMsg("")
    def strMsg: String_ => a = { _ => noMsg }
}


trait ErrorClassProxy[a] extends ErrorClass[a] {
    def selfErrorClass: ErrorClass[a]

    override def noMsg: a = selfErrorClass.noMsg
    override def strMsg: String_ => a = selfErrorClass.strMsg
}


object ErrorClass extends ErrorClassInstance {
    def apply[a](implicit i: ErrorClass[a]): ErrorClass[a] = i
}


sealed trait ErrorClassInstance { this: ErrorClass.type =>
    implicit val _ofString: ErrorClass[String_] = new ErrorClass[String_] {
        override def noMsg = ""
        override val strMsg = id[String_]
    }

    implicit val _ofIOError: ErrorClass[IOError] = new ErrorClass[IOError] {
        override val strMsg = IO.userError
    }
}
