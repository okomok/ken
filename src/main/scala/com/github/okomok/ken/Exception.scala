

// Copyright Shunsuke Sogame 2011.
//
// Copyright 2004, The University Court of the University of Glasgow.
// All rights reserved.
//
// Copyright (c) 2002 Simon Peyton Jones
//
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
    def `catch`[a](io: IO[a])(h: e => IO[a]): IO[a] = {
        val handler_ : SomeException => IORep[a] = e => fromException(e) match {
            case Just(e_) => IO.unIO(h(e_))
            case Nothing => throw e
        }
        IO { Prim.`catch`(IO.unIO(io))(handler_) }
    }

    type throwIO = e => IO[Nothing]
    def throwIO: throwIO = e => IO { Prim.raiseIO(toException(e)) }

    type `throw` = e => Nothing
    def `throw`: `throw` = e => Prim.raise(toException(e))
}


trait ExceptionProxy[a] extends Exception[a] with TypeableProxy[a] with ShowProxy[a] {
    def selfException: Exception[a]
    override def selfTypeable = selfException
    override def selfShow = selfException

    override def toException: toException = selfException.toException
    override def fromException: fromException = selfException.fromException

    override def `throw`: `throw` = selfException.`throw`
}


object Exception extends ExceptionInstance with ExceptionShortcut {
    def apply[a <: Kind.Function0](implicit i: Exception[a#apply0]): Exception[a#apply0] = i

    // ErrorCall
    //
    final case class ErrorCall(_1: String)
    object ErrorCall extends Eq.Of[ErrorCall] with Show.Of[ErrorCall] with Exception[ErrorCall] with ThisIsInstance {
        override val typeOf: typeOf = _ => implicitly[ClassManifest[ErrorCall]]
    }

    // catchAny
    //
    trait AnyExceptionHanlder[+a] {
        def apply[e](e: e)(implicit i: Exception[e]): IO[a]
    }

    def catchAny[a](io: IO[a])(hanlder: AnyExceptionHanlder[a]): IO[a] = {
        val handler_ : SomeException => IORep[a] = { case SomeException(e, i) => IO.unIO(hanlder(e)(i)) }
        IO { Prim.`catch`(IO.unIO(io))(handler_) }
    }
}


sealed trait ExceptionInstance { this: Exception.type =>
}


sealed trait ExceptionShortcut { this: Exception.type =>
    def toException[e](e: e)(implicit i: Exception[e]): SomeException = i.toException(e)
    def fromException[e](se: SomeException)(implicit i: Exception[e]): Maybe[e] = i.fromException(se)
    def `catch`[e, a](io: IO[a])(h: e => IO[a])(implicit i: Exception[e]): IO[a] = i.`catch`(io)(h)
    def `throw`[e](e: e)(implicit i: Exception[e]): Nothing = i.`throw`(e)
    def throwIO[e](e: e)(implicit i: Exception[e]): IO[Nothing] = i.throwIO(e)
}
