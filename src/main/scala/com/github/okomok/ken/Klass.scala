

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken



trait Klass0[+a] {
    type apply = a
}

trait Klass1[f[+_]] {
    type apply[+a] = f[a]
}

trait Klass2[f[+_, +_]] {
    type apply[+a, +b] = f[a, b]
}

trait Klass {
    // implicit def instance: this.type = this
}
