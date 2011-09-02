

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


/**
 * Namespace for monad transformers
 */
trait MonadTs[n[+_]] {
    val inner: Monad[n]

    protected implicit def innerFor[a](x: n[a]): inner.For[a] = inner.`for`(x)
    protected implicit def innerOp_>>=[a](x: n[a]): inner.Op_>>=[a] = inner.>>=(x)
}
