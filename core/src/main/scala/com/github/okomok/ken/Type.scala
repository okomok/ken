

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait Type[a] extends TypeEnvelope with Kind.const[a]


object Type {
    def apply[a]: Type[a] = new Type[a] {}
}
