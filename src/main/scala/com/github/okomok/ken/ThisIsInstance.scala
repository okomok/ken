

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait ThisIsInstance {
    final implicit val _instance: this.type = this
}
