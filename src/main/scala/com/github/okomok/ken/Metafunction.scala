

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Metafunction0 {
    type apply
}

trait Metafunction1 {
    type apply[+a]
}

trait Metafunction2 {
    type apply[+a, +b]
}
