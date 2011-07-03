

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Obj[x[+_], +y] {
    def obj: x[y]
}
