

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


/**
 * Works around that case class constructors return concrete types.
 */
trait Up[+a] extends Kind.constThis { self: a =>
    def up: a = self
}
