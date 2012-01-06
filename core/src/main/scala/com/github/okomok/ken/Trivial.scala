

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


// A typeclass which any type conforms.


trait Trivial[a] extends Typeclass[a] {
    final val asTrivial: Trivial[apply0] = this
}


trait TrivialProxy[a] {
    type selfTrivial = Trivial[a]
    def selfTrivial: selfTrivial
}


object Trivial extends TrivialInstance {
    def apply[a <: Kind.Function0](implicit _I: Trivial[a#apply0]): Trivial[a#apply0] = _I
}


sealed trait TrivialInstance { this: Trivial.type =>
    implicit def of[a]: Trivial[a] = new Trivial[a] {}
}
