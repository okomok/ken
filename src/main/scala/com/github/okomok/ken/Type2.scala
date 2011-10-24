

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait Type2[f[_, _]] extends TypeEnvelope


object Type2 {
    def apply[f[_, _]]: Type2[f] = new Type2[f] {}
}
