

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


final class &[+a](x: () => a) {
    private[this] lazy val v = x()
    def unary_! = v
}

object & {
    def apply[a](x: => a): &[a] = new &(() => x)

    implicit def toStrict[a](x: &[a]): a = !x
    implicit def fromFunction1[a1, r](f: a1 => r): &[a1] => &[r] = { x1 => &(f(!x1)) }
    implicit def fromFunction2[a1, a2, r](f: a1 => a2 => r): &[a1] => &[a2] => &[r] = { x1 => x2 => &(f(!x1)(!x2)) }
    implicit def fromFunction3[a1, a2, a3, r](f: a1 => a2 => a3 => r): &[a1] => &[a2] => &[a3] => &[r] = { x1 => x2 => x3 => &(f(!x1)(!x2)(!x3)) }

    def r[A, B, C](f: A => B => C)(x: A)(y: &[B]): C = f(x)(!y)
}


object ! {
    def unapply[a](x: &[a]): Option[a] = Some(!x)
}
