

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
    def strMsg: strMsg = _ => noMsg
}


trait ErrorClassProxy[a] extends ErrorClass[a] {
    def selfErrorClass: ErrorClass[a]

    override def noMsg: noMsg = selfErrorClass.noMsg
    override def strMsg: strMsg = selfErrorClass.strMsg
}


object ErrorClass extends ErrorClassInstance {
    def apply[a <: Kind.Function0](implicit i: ErrorClass[a#apply0]): ErrorClass[a#apply0] = i
}


sealed trait ErrorClassInstance { this: ErrorClass.type =>
    implicit val ofString: ErrorClass[String_] = new ErrorClass[String_] {
        override def noMsg = ""
        override val strMsg: strMsg = id
    }

    implicit val ofIOError: ErrorClass[IOError] = new ErrorClass[IOError] {
        override val strMsg: strMsg = IO.userError
    }

    implicit val ofAssertionError: ErrorClass[AssertionError] = new ErrorClass[AssertionError] {
        override val strMsg: strMsg = msg => new AssertionError(msg)
    }

    implicit def ofThrowable[x <: Throwable](implicit i: ClassManifest[x]): ErrorClass[x] = new ErrorClass[x] {
        override val strMsg: strMsg = msg => i.erasure.getConstructor(classOf[String]).newInstance(List.stringize(msg)).asInstanceOf[x]
    }
}
