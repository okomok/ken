

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


object Prelude {

    def identity[a](x: a): a = x

    def const[a](x: a)(y: Any): a = x

}
