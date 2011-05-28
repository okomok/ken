

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


object Prelude {

    def identity[a](x: a): a = x

    def const[a, b](x: a)(y: b): a = x

    def apply[a, b, c](x: a => b => c)(y: a)(z: b): c = x(y)(z)

    def flip[a, b, c](x: a => b => c)(y: b)(z: a): c = x(z)(y)

}
