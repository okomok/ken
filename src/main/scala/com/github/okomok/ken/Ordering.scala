

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


sealed abstract class Ordering

case object LT extends Ordering
case object EQ extends Ordering
case object GT extends Ordering


object Ordering extends Monoid[Ordering] {
// Overrides
    private[this] type m = Ordering
    override val mempty: m = EQ
    override val mappend: m => (=> m) => m = x => y => x match {
        case LT => LT
        case EQ => y
        case GT => GT
    }

// Instances
    implicit val monoid: Monoid[Ordering] = this
}
