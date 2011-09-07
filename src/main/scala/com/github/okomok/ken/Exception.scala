

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait Exception[e] extends Typeable[e] with Show[e] {
    final val asException: Exception[apply0] = this

    // Core
    //
    type toException = e => SomeException
    def toException: toException = e => SomeException(e)(this)

    type fromException = SomeException => Maybe[e]
    def fromException: fromException = { case SomeException(e, t) => Typeable.cast(e, Type[e])(t, this) }

    // Extra
    //
    type `throw` = e => Nothing
    def `throw`: `throw` = e => throw toException(e)
}


trait ExceptionProxy[a] extends Exception[a] with TypeableProxy[a] with ShowProxy[a] {
    def selfException: Exception[a]
    override def selfTypeable = selfException
    override def selfShow = selfException

    override def toException: toException = selfException.toException
    override def fromException: fromException = selfException.fromException
}


object Exception extends ExceptionInstance {
    def apply[a <: Kind.Function0](implicit i: Exception[a#apply0]): Exception[a#apply0] = i

    final case class ErrorCall(_1: String_)
    object ErrorCall extends Eq.Of[ErrorCall] with Show.Of[ErrorCall] with Exception[ErrorCall] with ThisIsInstance {
        override val typeOf: typeOf = _ => implicitly[ClassManifest[ErrorCall]]
    }
}


sealed trait ExceptionInstance { this: Exception.type =>
}
