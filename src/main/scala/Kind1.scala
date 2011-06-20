

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


import Monad.{`return`, >>=, mzero, _mplus_}


final class Kind1[m[_]] {

    case class Kleisli[a, b](runKleisli: a => m[b])

    object Kleisli {
        class ArrowInstance(implicit i: Monad[m]) extends Arrow[Kleisli] {
            protected[this] type cat[a, b] = Kleisli[a, b]
            override def id[a]: cat[a, a] = Kleisli { a => `return`(a) }
            override def op_<<<[a, b, c](f: cat[b, c])(g: cat[a, b]): cat[a, c] = Kleisli { b =>
                g.runKleisli(b) >>= f.runKleisli
            }

            protected[this] type a[a, b] = Kleisli[a, b]
            override def arr[b, c](f: b => c): a[b, c] = Kleisli { b => `return`(f(b)) }
            override def first[b, c, d](f: a[b, c]): a[(b, d), (c, d)] = Kleisli { case (b, d) =>
                f.runKleisli(b) >>= (c => `return`(c, d))
            }
            override def second[b, c, d](f: a[b, c]): a[(d, b), (d, c)] = Kleisli { case (d, b) =>
                f.runKleisli(b) >>= (c => `return`(d, c))
            }
        }

        class ArrowZeroInstance(implicit i: MonadPlus[m]) extends ArrowInstance with ArrowZero[Kleisli] {
            override def zeroArrow[b, c]: a[b, c] = Kleisli { _ => mzero }
        }

        class ArrowPlusInstance(implicit i: MonadPlus[m]) extends ArrowZeroInstance with ArrowPlus[Kleisli] {
            override def op_<+>[b, c](f: a[b, c])(g: => a[b, c]): a[b, c] = Kleisli { x =>
                f.runKleisli(x) _mplus_ g.runKleisli(x)
            }
        }

        implicit def arrowInstance(implicit i: Monad[m]): Arrow[Kleisli] = new ArrowInstance
        implicit def arrowZeroInstance(implicit i: MonadPlus[m]): ArrowZero[Kleisli] = new ArrowZeroInstance
        implicit def arrowPlusInstance(implicit i: MonadPlus[m]): ArrowPlus[Kleisli] = new ArrowPlusInstance
    }
}
