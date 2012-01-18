

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


// See: SI-2712, scalaz.Unapply.scala


trait Instance1[tc[_[+_]], fa] extends Kind.Unapply1 {
    def get: tc[apply1]
    final def apply(fa: fa): result1 = fa.asInstanceOf[result1]
}


object Instance1 {
    type Of[tc[_[+_]], f[+_], a] = Instance1[tc, f[a]] {
        type apply1[+a] = f[a]
        type arg1 = a
    }

    implicit def _of1[tc[_[+_]], f[+_], a1](implicit _T: tc[f]): Of[tc, f, a1] = new Instance1[tc, f[a1]] {
        override type apply1[+a] = f[a]
        override type arg1 = a1
        override def get: tc[apply1] = _T
    }

    implicit def _of2[tc[_[+_]], f[_, +_], a1, a2](implicit _T: tc[({type L[+a] = f[a1, a]})#L]): Of[tc, ({type L[+a] = f[a1, a]})#L, a2] = new Instance1[tc, f[a1, a2]] {
        override type apply1[+a] = f[a1, a]
        override type arg1 = a2
        override def get: tc[apply1] = _T
    }
}
