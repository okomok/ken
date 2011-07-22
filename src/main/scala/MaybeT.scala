

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


final class _MaybeTs[n[+_]](inner: Monad[n]) {

// Trans
    trait Trans[m[+_]] {
        def lift[a](n: n[a]): m[a]
    }

    object Trans {
        def apply[m[+_]](implicit i: Trans[m]): Trans[m] = i

        implicit val trivial: Trans[n] = new Trans[n] {
            override def lift[a](n: n[a]): n[a] = n
        }
    }

// MaybeT
    type _MaybeT[+a] = n[Maybe[a]]

    object _MaybeT {
        implicit val monad: MonadPlus[_MaybeT] with Trans[_MaybeT] = new MonadPlus[_MaybeT] with Trans[_MaybeT] {
            // Monad
            private[this] type m[+a] = _MaybeT[a]
            override def `return`[a](x: a): m[a] = inner.`return`(Just(x).up)
            override def op_>>=[a, b](x: m[a])(y: a => m[b]): m[b] = {
                import inner.>>=
                x >>= {
                    case Nothing => inner.`return`(Nothing.of[b])
                    case Just(v) => y(v)
                }
            }
            // MonadPlus
            override def mzero: m[Nothing] = inner.`return`(Nothing)
            override def mplus[a](x: m[a])(y: => m[a]): m[a] = {
                import inner.>>=
                x >>= {
                    case Nothing => y
                    case Just(_) => x
                }
            }
            // Trans
            override def lift[a](n: n[a]): m[a] = inner.liftM(Maybe.just[a])(n)
        }
    }
}
