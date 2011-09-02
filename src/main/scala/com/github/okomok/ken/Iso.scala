

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


// Higher kinded =:=
//   See: =~~= of scalaz7


trait Iso[f[+_], g[+_]] extends Typeclass {
    final val asIso: Iso[f, g] = this

    implicit def imply[a](f: f[a]): g[a]
    implicit def unimply[a](g: g[a]): f[a]
}


object Iso extends IsoInstance {
    def apply[f <: Kind.Function1, g <: Kind.Function1](implicit i: Iso[f#apply, g#apply]): Iso[f#apply, g#apply] = i
}


private[ken] trait IsoInstance0 { this: Iso.type =>
    implicit def ofDual[f[+_], g[+_]](implicit i: Iso[g, f]): Iso[f, g] = new Iso[f, g] {
        implicit def imply[a](f: f[a]): g[a] = i.unimply(f)
        implicit def unimply[a](g: g[a]): f[a] = i.imply(g)
    }

    // needed unfortunately.
    implicit val ofWeakIdentity: Iso[WeakIdentity.apply, ({type g[+a] = a})#g] = new Iso[WeakIdentity.apply, ({type g[+a] = a})#g] {
        private type f[+a] = a
        private type g[+a] = a
        implicit def imply[a](f: f[a]): g[a] = f
        implicit def unimply[a](g: g[a]): f[a] = g
    }
}

sealed trait IsoInstance extends IsoInstance0 { this: Iso.type =>
    implicit def ofTrivial[f[+_]]: Iso[f, f] = new Iso[f, f] {
        private type g[a] = f[a]
        implicit def imply[a](f: f[a]): g[a] = f
        implicit def unimply[a](g: g[a]): f[a] = g
    }
}
