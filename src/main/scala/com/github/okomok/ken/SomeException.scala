

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


sealed class SomeException(private val e: Any, private val i: Typeable[Any]) extends Throwable

object SomeException {
    def apply[e](e: e)(implicit i: Exception[e]): SomeException = new SomeException(e, erasure(i))
    def unapply[e](x: SomeException): Option[(Any, Typeable[Any])] = Some((x.e, x.i))

    private def erasure[e](i: Typeable[e]): Typeable[Any] = new Typeable[Any] {
        override val typeOf: typeOf = _ => i.typeOf(Lazy(error("unused")))
    }
}
