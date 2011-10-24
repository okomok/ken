

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait Type1[f[_]] extends TypeEnvelope


object Type1 {
    def apply[f[_]]: Type1[f] = new Type1[f] {}
}
