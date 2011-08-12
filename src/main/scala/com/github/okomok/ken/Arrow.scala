

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Arrow[a[-_, +_]] extends Category[a] {
    final val asArrow: Arrow[apply] = this
    // Core
    //
    def arr[b, c](f: b => c): a[b, c]
    def first[b, c, d](f: a[b, c]): a[(b, d), (c, d)]
    def second[b, c, d](f: a[b, c]): a[(d, b), (d, c)] = {
        def swap[x, y](v: (x, y)): (y, x) = (v._2, v._1)
        arr(swap[d, b]) >>> first(f) >>> arr(swap[c, d])
    }

    // Overrides
    //
    // Category
    override def cid[a_]: a[a_, a_] = arr(ken.id[a_])

    // Extra
    //
    def op_***[b, c, b_, c_](f: a[b, c])(g: a[b_, c_]): a[(b, b_), (c, c_)] = first(f) >>> second(g)
    def op_&&&[b, c, c_](f: a[b, c])(g: a[b, c_]): a[b, (c, c_)] = arr((b: b) => (b, b)) >>> f *** g

    def op_^>>[b, c, d](f: b => c)(a: a[c, d]): a[b, d] = arr(f) >>> a
    def op_>>^[b, c, d](a: a[b, c])(f: c => d): a[b, d] = a >>> arr(f)
    def op_<<^[b, c, d](a: a[c, d])(f: b => c): a[b, d] = a <<< arr(f)
    def op_^<<[b, c, d](f: c => d)(a: a[b, c]): a[b, d] = arr(f) <<< a

    // Infix
    //
    sealed class Op_***[b, c](f: a[b, c]) {
        def ***[b_, c_](g: a[b_, c_]): a[(b, b_), (c, c_)] = op_***(f)(g)
    }
    final implicit def ***[b, c](f: a[b, c]): Op_***[b, c] = new Op_***[b, c](f)

    sealed class Op_&&&[b, c](f: a[b, c]) {
        def &&&[c_](g: a[b, c_]): a[b, (c, c_)] = op_&&&(f)(g)
    }
    final implicit def &&&[b, c](f: a[b, c]): Op_&&&[b, c] = new Op_&&&[b, c](f)

    final def returnA[b]: a[b, b] = arr(ken.id[b])
    sealed class Op_^>>[b, c](f: b => c) {
        def ^>>[d](a: a[c, d]): a[b, d] = op_^>>(f)(a)
    }
    final implicit def ^>>[b, c](f: b => c): Op_^>>[b, c] = new Op_^>>[b, c](f)

    sealed class Op_>>^[b, c](a: a[b, c]) {
        def >>^[d](f: c => d): a[b, d] = op_>>^(a)(f)
    }
    final implicit def >>^[b, c](a: a[b, c]): Op_>>^[b, c] = new Op_>>^[b, c](a)

    sealed class Op_<<^[c, d](a: a[c, d]) {
        def <<^[b](f: b => c): a[b, d] = op_<<^(a)(f)
    }
    final implicit def <<^[c, d](a: a[c, d]): Op_<<^[c, d] = new Op_<<^[c, d](a)

    sealed class Op_^<<[c, d](f: c => d) {
        def ^<<[b](a: a[b, c]): a[b, d] = op_^<<(f)(a)
    }
    final implicit def ^<<[c, d](f: c => d): Op_^<<[c, d] = new Op_^<<[c, d](f)
}


trait ArrowProxy[a[-_, +_]] extends Arrow[a] with CategoryProxy[a] {
    override def self: Arrow[a]

    override def arr[b, c](f: b => c): a[b, c] = self.arr(f)
    override def first[b, c, d](f: a[b, c]): a[(b, d), (c, d)] = self.first(f)
    override def second[b, c, d](f: a[b, c]): a[(d, b), (d, c)] = self.second(f)

    override def op_***[b, c, b_, c_](f: a[b, c])(g: a[b_, c_]): a[(b, b_), (c, c_)] = self.op_***(f)(g)
    override def op_&&&[b, c, c_](f: a[b, c])(g: a[b, c_]): a[b, (c, c_)] = self.op_&&&(f)(g)
    override def op_^>>[b, c, d](f: b => c)(a: a[c, d]): a[b, d] = self.op_^>>(f)(a)
    override def op_>>^[b, c, d](a: a[b, c])(f: c => d): a[b, d] = self.op_>>^(a)(f)
    override def op_<<^[b, c, d](a: a[c, d])(f: b => c): a[b, d] = self.op_<<^(a)(f)
    override def op_^<<[b, c, d](f: c => d)(a: a[b, c]): a[b, d] = self.op_^<<(f)(a)
}


object Arrow {
    def apply[a <: Kind.Function2](implicit i: Arrow[a#apply]): Arrow[a#apply] = i
}


/*
    private[this] class OfKleisliArrow[m[_]](implicit i: Monad[m]) extends Arrow[({type a[a, b] = a => m[b]})#a] {
        // Category
        protected[this] type cat[a, b] = a => m[b]
        override def cid[a]: cat[a, a] = { a => `return`(a) }
        override def op_<<<[a, b, c](f: cat[b, c])(g: cat[a, b]): cat[a, c] = { b =>
            g(b) >>= f
        }
        // Arrow
        protected[this] type a[a, b] = cat[a, b]
        override def arr[b, c](f: b => c): a[b, c] = { b => `return`(f(b)) }
        override def first[b, c, d](f: a[b, c]): a[(b, d), (c, d)] = { case (b, d) =>
            f(b) >>= (c => `return`(c, d))
        }
        override def second[b, c, d](f: a[b, c]): a[(d, b), (d, c)] = { case (d, b) =>
            f(b) >>= (c => `return`(d, c))
        }
    }
    implicit def _ofKleisliArrow[m[_]](implicit i: Monad[m]): Arrow[({type a[a, b] = a => m[b]})#a] = new OfKleisliArrow[m]

    private[this] class OfKleisliArrowZero[m[_]](implicit i: MonadPlus[m]) extends OfKleisliArrow[m] with ArrowZero[({type a[a, b] = a => m[b]})#a] {
        override def zeroArrow[b, c]: a[b, c] = _ => mzero
    }
    implicit def _ofKleisliArrowZero[m[_]](implicit i: MonadPlus[m]): ArrowZero[({type a[a, b] = a => m[b]})#a] = new OfKleisliArrowZero[m]

    implicit def _ofKleisliArrowPlus[m[_]](implicit i: MonadPlus[m]): ArrowPlus[({type a[a, b] = a => m[b]})#a] = new OfKleisliArrowZero[m] with ArrowPlus[({type a[a, b] = a => m[b]})#a] {
        override def op_<+>[b, c](f: a[b, c])(g: => a[b, c]): a[b, c] = { x => f(x) _mplus_ g(x) }
    }
*/
