

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait ThisIsInstance {
    final implicit val instance: this.type = this
}
