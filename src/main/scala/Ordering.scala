

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


sealed abstract class Ordering

case object LT extends Ordering
case object EQ extends Ordering
case object GT extends Ordering
