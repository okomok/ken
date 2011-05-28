

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok


package object ken {

    def id[a](x: a): a = x

    def const[a, b](x: a)(y: b): a = x

    def apply[a, b](x: a => b)(y: a): b = x(y)

    def flip[a, b, c](x: a => b => c)(y: b)(z: a): c = x(z)(y)

}
