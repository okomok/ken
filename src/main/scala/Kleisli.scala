

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


import Monad.{`return`, >>=}
import MonadPlus.{_mplus_}


final class Kleisli[m[_]] {
    case class Apply[a, b](runKleisli: a => m[b])

    object Apply {
        class ArrowInstance(implicit i: Monad[m]) extends Arrow[Apply] {
            protected[this] type cat[a, b] = Apply[a, b]
            override def id[a]: cat[a, a] = Apply { a => `return`(a) }
            override def op_<<<[a, b, c](f: cat[b, c])(g: cat[a, b]): cat[a, c] = Apply { b =>
                g.runKleisli(b) >>= f.runKleisli
            }

            protected[this] type a[a, b] = Apply[a, b]
            override def arr[b, c](f: b => c): a[b, c] = Apply { b => `return`(f(b)) }
            override def first[b, c, d](f: a[b, c]): a[(b, d), (c, d)] = Apply { case (b, d) =>
                f.runKleisli(b) >>= (c => `return`(c, d))
            }
            override def second[b, c, d](f: a[b, c]): a[(d, b), (d, c)] = Apply { case (d, b) =>
                f.runKleisli(b) >>= (c => `return`(d, c))
            }
        }

        class ArrowZeroInstance(implicit i: MonadPlus[m]) extends ArrowInstance with ArrowZero[Apply] {
            override def zeroArrow[b, c]: a[b, c] = Apply { _ => MonadPlus.mzero }
        }

        class ArrowPlusInstance(implicit i: MonadPlus[m]) extends ArrowZeroInstance with ArrowPlus[Apply] {
            override def op_<+>[b, c](f: a[b, c])(g: => a[b, c]): a[b, c] = Apply { x =>
                f.runKleisli(x) _mplus_ g.runKleisli(x)
            }
        }

        implicit def arrowInstance(implicit i: Monad[m]): Arrow[Apply] = new ArrowInstance
        implicit def arrowZeroInstance(implicit i: MonadPlus[m]): ArrowZero[Apply] = new ArrowZeroInstance
        implicit def arrowPlusInstance(implicit i: MonadPlus[m]): ArrowPlus[Apply] = new ArrowPlusInstance
    }
}
