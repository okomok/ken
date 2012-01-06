

// Copyright Shunsuke Sogame 2011.
//
// Copyright 2004, The University Court of the University of Glasgow.
// All rights reserved.
//
// Copyright (c) 2002 Simon Peyton Jones
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


object Rational {
    def apply(x: Integer, y: Integer): Rational = Ratio(x, y)
    def unapply(r: Rational): Option[(Integer, Integer)] = Some((r.numerator, r.denominator))
}
