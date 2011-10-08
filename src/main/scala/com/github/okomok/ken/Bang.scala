

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


object ! {
    def unapply[a](x: Eval[a]): Option[a] = Some(x._eval)
}
