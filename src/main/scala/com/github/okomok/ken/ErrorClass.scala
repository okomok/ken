

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait ErrorClass[a] extends Typeclass0[a] {
    final val asErrorClass: ErrorClass[apply0] = this

    // Core
    //
    type noMsg = a
    def noMsg: a = strMsg("")

    type strMsg = String_ => a
    def strMsg: strMsg = { _ => noMsg }
}


trait ErrorClassProxy[a] extends ErrorClass[a] {
    def selfErrorClass: ErrorClass[a]

    override def noMsg: noMsg = selfErrorClass.noMsg
    override def strMsg: strMsg = selfErrorClass.strMsg
}


object ErrorClass extends ErrorClassInstance {
    def apply[a](implicit i: ErrorClass[a]): ErrorClass[a] = i
}


sealed trait ErrorClassInstance { this: ErrorClass.type =>
    implicit val ofString: ErrorClass[String_] = new ErrorClass[String_] {
        override def noMsg = ""
        override val strMsg = id[String_]
    }

    implicit val ofIOError: ErrorClass[IOError] = new ErrorClass[IOError] {
        override val strMsg = IO.userError
    }
}
