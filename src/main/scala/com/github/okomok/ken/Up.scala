

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


/**
 * Works around that case class constructors return concrete types.
 */
trait Up[+a] { self: a =>
    def up: a = self
}
