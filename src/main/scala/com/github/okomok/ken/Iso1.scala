

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


object Iso1 extends Iso1Instance {
    def apply[f <: Kind.Function1, g <: Kind.Function1](implicit i: Iso1[f#apply, g#apply]): Iso1[f#apply, g#apply] = i
}


private[ken] trait Iso1Instance0 { this: Iso1.type =>
    implicit def ofDual[f[+_], g[+_]](implicit i: Iso1[g, f]): Iso1[f, g] = new Iso1[f, g] {
        implicit def imply[a](f: f[a]): g[a] = i.unimply(f)
        implicit def unimply[a](g: g[a]): f[a] = i.imply(g)
    }
}

sealed trait Iso1Instance extends Iso1Instance0 { this: Iso1.type =>
    implicit def ofTrivial[f[+_]]: Iso1[f, f] = new Iso1[f, f] {
        private type g[a] = f[a]
        implicit def imply[a](f: f[a]): g[a] = f
        implicit def unimply[a](g: g[a]): f[a] = g
    }
}
