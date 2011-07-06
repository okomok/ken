

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


final case class Kleisli[m[+_], a, +b](runKleisli: a => m[b])


object Kleisli {
    private[this] class ArrowInstance[m[+_]](implicit i: Monad[m]) extends Arrow[({type a[a, b] = Kleisli[m, a, b]})#a] {
        import i.{method, `return`}

        protected[this] type cat[a, b] = Kleisli[m, a, b]
        override def cid[a]: cat[a, a] = Kleisli { a => `return`(a) }
        override def op_<<<[a, b, c](f: cat[b, c])(g: cat[a, b]): cat[a, c] = Kleisli { b =>
            g.runKleisli(b) >>= f.runKleisli
        }

        protected[this] type a[a, b] = Kleisli[m, a, b]
        override def arr[b, c](f: b => c): a[b, c] = Kleisli { b => `return`(f(b)) }
        override def first[b, c, d](f: a[b, c]): a[(b, d), (c, d)] = Kleisli { case (b, d) =>
            f.runKleisli(b) >>= (c => `return`(c, d))
        }
        override def second[b, c, d](f: a[b, c]): a[(d, b), (d, c)] = Kleisli { case (d, b) =>
            f.runKleisli(b) >>= (c => `return`(d, c))
        }
    }

    private[this] class ArrowZeroInstance[m[+_]](implicit i: MonadPlus[m]) extends ArrowInstance[m] with ArrowZero[({type a[a, b] = Kleisli[m, a, b]})#a] {
        override def zeroArrow[b, c]: a[b, c] = Kleisli { _ => i.mzero }
    }

    private[this] class ArrowPlusInstance[m[+_]](implicit i: MonadPlus[m]) extends ArrowZeroInstance[m] with ArrowPlus[({type a[a, b] = Kleisli[m, a, b]})#a] {
        override def op_<+>[b, c](f: a[b, c])(g: => a[b, c]): a[b, c] = Kleisli { x =>
            import i.method
            f.runKleisli(x) _mplus_ g.runKleisli(x)
        }
    }

    implicit def arrow[m[+_]](implicit i: Monad[m]): Arrow[({type a[a, b] = Kleisli[m, a, b]})#a] = new ArrowInstance[m]
    implicit def arrowZero[m[+_]](implicit i: MonadPlus[m]): ArrowZero[({type a[a, b] = Kleisli[m, a, b]})#a] = new ArrowZeroInstance[m]
    implicit def arrowPlus[m[+_]](implicit i: MonadPlus[m]): ArrowPlus[({type a[a, b] = Kleisli[m, a, b]})#a] = new ArrowPlusInstance[m]
}
