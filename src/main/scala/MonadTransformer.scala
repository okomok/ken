

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


// deprecated
trait MonadTransfomer[m[+_]] {
    implicit def inner: Monad[m]
}
