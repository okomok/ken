

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


final case class Ratio[a](numerator: a, denominator:a)(implicit i: Integral[a])

object Ratio {

    final val ratioPrec = 7
    final val ratioPrec1 = 8


}
