

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


final case class SomeException(rep: (e, Exception[e]) forSome { type e }) extends Throwable


object SomeException extends Exception[SomeException] with TypeableProxy[SomeException] with ThisIsInstance {
    def apply[e](e: e)(implicit i: Exception[e]): SomeException = new SomeException(e, i)
    // scalac-generated `unapply` seems to have a bug.

    // Overrides
    //
    // Typeable
    override val selfTypeable = Typeable.of[SomeException]
    // Exception
    override val toException: toException = se => se
    override val fromException: fromException = se => Just(se)
}
