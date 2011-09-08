

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


sealed class SomeException(private val e: Any, private val i: Exception[Any]) extends Throwable

object SomeException {
    def apply[e](e: e)(implicit i: Exception[e]): SomeException = new SomeException(e, erasure(i))
    def unapply[e](x: SomeException): Option[(Any, Exception[Any])] = Some((x.e, x.i))

    private def erasure[e](i: Exception[e]): Exception[Any] = new Exception[Any] {
        override val typeOf: typeOf = _ => i.typeOf(Lazy(error("unused")))
    }
}
