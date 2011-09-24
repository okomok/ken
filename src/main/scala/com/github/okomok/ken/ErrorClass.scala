

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

    type strMsg = String => a
    def strMsg: strMsg = _ => noMsg
}


trait ErrorClassProxy[a] extends ErrorClass[a] {
    def selfErrorClass: ErrorClass[a]

    override def noMsg: noMsg = selfErrorClass.noMsg
    override def strMsg: strMsg = selfErrorClass.strMsg
}


object ErrorClass extends ErrorClassInstance with ErrorClassShortcut {
    def apply[a <: Kind.Function0](implicit i: ErrorClass[a#apply0]): ErrorClass[a#apply0] = i

    def deriving[nt <: Kind.Newtype0](implicit i: ErrorClass[nt#oldtype0], j: Newtype0[nt#apply0, nt#oldtype0, _]): ErrorClass[nt#apply0] = new ErrorClass[nt#apply0] {
        override val noMsg: noMsg = j.newOf(i.noMsg)
        override val strMsg: strMsg = msg => j.newOf(i.strMsg(msg))
    }

    def weak[nt <: Kind.Newtype0](implicit i: ErrorClass[nt#apply0], j: Newtype0[nt#apply0, nt#oldtype0, _]): ErrorClass[nt#oldtype0] = deriving[Kind.coNewtype0[nt]](i, j.coNewtype)
}


sealed trait ErrorClassInstance { this: ErrorClass.type =>
    implicit val ofString: ErrorClass[String] = new ErrorClass[String] {
        override def noMsg = ""
        override val strMsg: strMsg = id
    }

    implicit val ofAssertionError: ErrorClass[AssertionError] = new ErrorClass[AssertionError] {
        override val strMsg: strMsg = msg => new AssertionError(msg)
    }

    implicit def ofThrowable[x <: Throwable](implicit i: ClassManifest[x]): ErrorClass[x] = new ErrorClass[x] {
        override val strMsg: strMsg = msg => i.erasure.getConstructor(classOf[JString]).newInstance(List.toJString(msg)).asInstanceOf[x]
    }
}


sealed trait ErrorClassShortcut { this: ErrorClass.type =>
    def noMsg[a](implicit i: ErrorClass[a]): a = i.noMsg
    def strMsg[a](s: String)(implicit i: ErrorClass[a]): a = i.strMsg(s)
}
