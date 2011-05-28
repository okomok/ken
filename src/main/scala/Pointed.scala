

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Pointed extends Functor {
    def pure[a](x: => a): f_[a]
}


object Pointed {

    trait OfList extends Pointed with Functor.OfList {
        override def pure[a](x: => a): f_[a] = x :: Nil
    }
    val OfList = new OfList {}

    trait OfOption extends Pointed with Functor.OfOption {
        override def pure[a](x: => a): f_[a] = Some(x)
    }
    val OfOption = new OfOption {}

}
