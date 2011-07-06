

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


// Needed?
trait Klass {
    implicit def instance: this.type = this
}
