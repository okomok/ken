

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Typeable[a] {
    def typeOf(x: => a): ClassManifest[a] // Notice `x` isn't evaluated.

    final def cast[b](x: a)(implicit bc: Typeable[b]): Maybe[b] = {
        lazy val r: Maybe[b] = if (typeOf(x) <:< bc.typeOf(Maybe.fromJust(r))) {
            Maybe.Just(x.asInstanceOf[b])
        } else {
            Maybe.Nothing
        }
        r
    }
}

object Typeable {
    implicit def instanceOfAny[a](implicit ac: ClassManifest[a]): Typeable[a] = new Typeable[a] {
        override def typeOf(x: => a): ClassManifest[a] = ac
    }
}
