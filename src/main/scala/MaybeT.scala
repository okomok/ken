

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


import Monad.>>=


final case class MaybeT[n[+_], +a](runMaybeT: n[Maybe[a]]) extends MonadPlusObj[({type m[+a] = MaybeT[n, a]})#m, a] {
    override val obj = this
}


object MaybeT {
    def runMaybeT[n[+_], a](x: MaybeT[n, a]): n[Maybe[a]] = x.runMaybeT

    implicit def monad[n[+_]](implicit i: Monad[n]): MonadPlus[({type m[a] = MaybeT[n, a]})#m] = new MonadPlus[({type m[a] = MaybeT[n, a]})#m] {
        private[this] type m[a] = MaybeT[n, a]
        // Monad
        override def `return`[a](x: a): m[a] = MaybeT { Monad.`return`(Just(x).up)(i) }
        override def op_>>=[a, b](x: m[a])(y: a => m[b]): m[b] = MaybeT {
            runMaybeT(x) >>= {
                case Nothing => Monad.`return`(Nothing.of[b])(i)
                case Just(v) => runMaybeT(y(v))
            }
        }
        // MonadPlus
        override def mzero[a]: m[a] = MaybeT { Monad.`return`(Nothing.of[a])(i) }
        override def mplus[a](x: m[a])(y: => m[a]): m[a] = MaybeT {
            runMaybeT(x) >>= {
                case Nothing => runMaybeT(y)
                case Just(_) => runMaybeT(x)
            }
        }
    }

    implicit val monadTrans: MonadTrans[MaybeT] = new MonadTrans[MaybeT] {
        private[this] type t[m[+_], +a] = MaybeT[m, a]
        def lift[m[+_], a](x: m[a])(implicit i: Monad[m]): t[m, a] = MaybeT { Monad.liftM(Maybe.just[a])(x) }
    }
}
