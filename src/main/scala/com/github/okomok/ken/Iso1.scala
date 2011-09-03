

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


// Higher kinded =:=
//   See: =~~= of scalaz7


trait Iso1[f[+_], g[+_]] extends Typeclass {
    final val asIso: Iso1[f, g] = this

    implicit def imply[a](f: f[a]): g[a]
    implicit def unimply[a](g: g[a]): f[a]
}


trait Iso1Proxy[f[+_], g[+_]] extends Iso1[f, g] {
    def selfIso1: Iso1[f, g]

    override def imply[a](f: f[a]): g[a] = selfIso1.imply(f)
    override def unimply[a](g: g[a]): f[a] = selfIso1.unimply(g)
}


object Iso1 extends Iso1Instance {
    def apply[f <: Kind.Function1, g <: Kind.Function1](implicit i: Iso1[f#apply, g#apply]): Iso1[f#apply, g#apply] = i
}


private[ken] trait Iso1Instance0 { this: Iso1.type =>
    implicit def ofTransivity[f[+_], h[+_], g[+_]](implicit i: Iso1[f, h], j: Iso1[h, g]): Iso1[f, g] = new Iso1[f, g] {
        override def imply[a](f: f[a]): g[a] = j.imply(i.imply(f))
        override def unimply[a](g: g[a]): f[a] = i.unimply(j.unimply(g))
    }
}

private[ken] trait Iso1Instance1 { this: Iso1.type =>
    implicit def ofSymmetry[f[+_], g[+_]](implicit i: Iso1[g, f]): Iso1[f, g] = new Iso1[f, g] {
        override def imply[a](f: f[a]): g[a] = i.unimply(f)
        override def unimply[a](g: g[a]): f[a] = i.imply(g)
    }
}

sealed trait Iso1Instance extends Iso1Instance1 { this: Iso1.type =>
    implicit def ofReflexivity[f[+_]]: Iso1[f, f] = new Iso1[f, f] {
        private type g[a] = f[a]
        override def imply[a](f: f[a]): g[a] = f
        override def unimply[a](g: g[a]): f[a] = g
    }
}
