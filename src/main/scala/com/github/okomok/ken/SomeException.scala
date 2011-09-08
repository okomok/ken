

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


sealed class SomeException(private val e: Any, private val i: Exception[Any]) extends Throwable


object SomeException extends Eq.Of[SomeException] with Show.Of[SomeException]
    with Exception[SomeException] with TypeableProxy[SomeException] with ThisIsInstance
{
    def apply[e](e: e)(implicit i: Exception[e]): SomeException = new SomeException(e, erasure(i))
    def unapply[e](x: SomeException): Option[(Any, Exception[Any])] = Some((x.e, x.i))

    private def erasure[e](i: Exception[e]): Exception[Any] = new Exception[Any] {
        override val typeOf: typeOf = _ => i.typeOf(Lazy(error("unused")))
    }

    // Overrides
    //
    // Typeable
    override val selfTypeable = Typeable.of[SomeException]
    // Exception
    override val toException: toException = se => se
    override val fromException: fromException = se => Just(se)
}
