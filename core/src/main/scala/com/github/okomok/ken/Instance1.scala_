

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


// See: scalaz.Unapply


package com.github.okomok
package ken


trait Instance1[c[_[+_]], t] extends Kind.Unapply1 {
    val instance: c[apply1]
    def apply(t: t): apply1[arg1]
}


object Instance1 extends Instance1Instance


private[ken] sealed trait Instance1Instance0 { this: Instance1.type =>
    implicit def _toInstance[c[_[+_]], t](from: Instance1[c, t]): c[from.apply1] = from.instance

    implicit def _of1[c[_[+_]], f[+_], a1](implicit c: c[f]): Instance1[c, f[a1]] = new Instance1[c, f[a1]] {
        override type apply1[+x] = f[x]
        override type arg1 = a1
        override val instance: c[apply1] = c
        override def apply(t: f[a1]): apply1[arg1] = t
    }

    implicit def _of2[c[_[+_]], f[_, +_], a1, a2](implicit c: c[({type g[+x] = f[a1, x]})#g]): Instance1[c, f[a1, a2]] = new Instance1[c, f[a1, a2]] {
        override type apply1[+x] = f[a1, x]
        override type arg1 = a2
        override val instance: c[apply1] = c
        override def apply(t: f[a1, a2]): apply1[arg1] = t
    }
}

sealed trait Instance1Instance extends Instance1Instance0 { this: Instance1.type =>
    // For monad-transformers
    //
    implicit def _of2T[c[_[+_]], f[_[+_], +_], a1[+_], a2](implicit c: c[({type g[+x] = f[a1, x]})#g]): Instance1[c, f[a1, a2]] = new Instance1[c, f[a1, a2]] {
        override type apply1[+x] = f[a1, x]
        override type arg1 = a2
        override val instance: c[apply1] = c
        override def apply(t: f[a1, a2]): apply1[arg1] = t
    }

    implicit def _of3T[c[_[+_]], f[_, _[+_], +_], a1, a2[+_], a3](implicit c: c[({type g[+x] = f[a1, a2, x]})#g]): Instance1[c, f[a1, a2, a3]] = new Instance1[c, f[a1, a2, a3]] {
        override type apply1[+x] = f[a1, a2, x]
        override type arg1 = a3
        override val instance: c[apply1] = c
        override def apply(t: f[a1, a2, a3]): apply1[arg1] = t
    }
}
