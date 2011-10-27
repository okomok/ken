

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait GenericL[c[_]] {
    def apply[d, b](c: c[d => b])(d: d)(implicit ii: Data[d]): c[b]
}
