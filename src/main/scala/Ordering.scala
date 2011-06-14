

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


sealed abstract class Ordering

object LT extends Ordering
object EQ extends Ordering
object GT extends Ordering
