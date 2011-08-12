

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


/**
 * Implements `d` instances from `p` instances.
 */
trait Imply0[p, d] extends Imply1[({type p_[+a] = p})#p_, ({type d_[+a] = d})#d_] {
    final val asImply0: Imply0[p, d] = this

    // Core
    //
    def imply0(p: p): d
    def unimply0(d: => d): p

    // Overrides
    //
    // Imply1
    override def imply1[a](p: p): d = imply0(p)
    override def unimply1[a](d: => d): p = unimply0(d)
}


trait Imply0Proxy[p, d] extends Imply0[p, d] with Proxy {
    override def self: Imply0[p, d]

    override def imply0(p: p): d = self.imply0(p)
    override def unimply0(d: => d): p = self.unimply0(d)
}


object Imply0 {
    def apply[p, d](implicit i: Imply0[p, d]): Imply0[p, d] = i
}
