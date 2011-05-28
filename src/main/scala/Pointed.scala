

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Pointed extends Functor {
    def pure[a](x: => a): f_[a]
}
