

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait Pure[c[_]] {
    def apply[g](g: g): c[g]
}
