

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2010 John Millikin
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package enumerator


// For conversation between inner types.
//
trait Step_[-a, n[+_], +b]

trait Continue_Tag
trait Yield_Tag
trait Error_Tag

trait Continue_[a, n[+_], b] extends Step_[a, n, b] with Continue_Tag {
    type k = Stream[a] => NewtypeOf[n[Step_[a, n, b]]]
    def k: k
}

trait Yield_[a, n[+_], b] extends Step_[a, n, b] with Yield_Tag {
    def x: b
    def extra: Stream[a]
}

trait Error_[n[+_]] extends Step_[Any, n, Nothing] with Error_Tag {
    def err: SomeException
}
