

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Arrow[a[_, _]] extends Category[a] {
    def arr[b, c](f: b => c): a[b, c]
    def first[b, c, d](f: a[b, c]): a[(b, d), (c, d)]
    def second[b, c, d](f: a[b, c]): a[(d, b), (d, c)] = {
        def swap[x, y](v: (x, y)): (y, x) = (v._2, v._1)
        arr(swap[d, b]) >>> first(f) >>> arr(swap[c, d])
    }

    def op_***[b, c, b_, c_](f: a[b, c])(g: a[b_, c_]): a[(b, b_), (c, c_)] = first(f) >>> second(g)
    def op_&&&[b, c, c_](f: a[b, c])(g: a[b, c_]): a[b, (c, c_)] = arr((b: b) => (b, b)) >>> f *** g

    override def cid[a_]: a[a_, a_] = arr(ken.id[a_])

    final private[ken] class Op_***[b, c](f: a[b, c]) {
        def ***[b_, c_](g: a[b_, c_]): a[(b, b_), (c, c_)] = op_***(f)(g)
    }
    final implicit def ***[b, c](f: a[b, c]): Op_***[b, c] = new Op_***[b, c](f)

    final private[ken] class Op_&&&[b, c](f: a[b, c]) {
        def &&&[c_](g: a[b, c_]): a[b, (c, c_)] = op_&&&(f)(g)
    }
    final implicit def &&&[b, c](f: a[b, c]): Op_&&&[b, c] = new Op_&&&[b, c](f)

    final def returnA[b]: a[b, b] = arr(ken.id[b])

    final def op_^>>[b, c, d](f: b => c)(a: a[c, d]): a[b, d] = arr(f) >>> a
    final def op_>>^[b, c, d](a: a[b, c])(f: c => d): a[b, d] = a >>> arr(f)
    final def op_<<^[b, c, d](a: a[c, d])(f: b => c): a[b, d] = a <<< arr(f)
    final def op_^<<[b, c, d](f: c => d)(a: a[b, c]): a[b, d] = arr(f) <<< a

    final private[ken] class Op_^>>[b, c](f: b => c) {
        def ^>>[d](a: a[c, d]): a[b, d] = op_^>>(f)(a)
    }
    final implicit def ^>>[b, c](f: b => c): Op_^>>[b, c] = new Op_^>>[b, c](f)

    final private[ken] class Op_>>^[b, c](a: a[b, c]) {
        def >>^[d](f: c => d): a[b, d] = op_>>^(a)(f)
    }
    final implicit def >>^[b, c](a: a[b, c]): Op_>>^[b, c] = new Op_>>^[b, c](a)

    final private[ken] class Op_<<^[c, d](a: a[c, d]) {
        def <<^[b](f: b => c): a[b, d] = op_<<^(a)(f)
    }
    final implicit def <<^[c, d](a: a[c, d]): Op_<<^[c, d] = new Op_<<^[c, d](a)

    final private[ken] class Op_^<<[c, d](f: c => d) {
        def ^<<[b](a: a[b, c]): a[b, d] = op_^<<(f)(a)
    }
    final implicit def ^<<[c, d](f: c => d): Op_^<<[c, d] = new Op_^<<[c, d](f)
}


trait ArrowZero[a[_, _]] extends Arrow[a] {
    def zeroArrow[b, c]: a[b, c]
}

trait ArrowPlus[a[_, _]] extends ArrowZero[a] {
    def op_<+>[b, c](f: a[b, c])(g: => a[b, c]): a[b, c]

    final private[ken] class Op_<+>[b, c](f: a[b, c]) {
        def <+>(g: => a[b, c]): a[b, c] = op_<+>(f)(g)
    }
    final implicit def <+>[b, c](f: a[b, c])(implicit i: ArrowPlus[a]): Op_<+>[b, c] = new Op_<+>[b, c](f)
}

trait ArrowChoice[a[_, _]] extends Arrow[a] {
    private[this] implicit val i = this

    def left[b, c, d](f: a[b, c]): a[Either[b, d], Either[c, d]]

    def right[b, c, d](f: a[b, c]): a[Either[d, b], Either[d, c]] = {
        def mirror[x, y](v: Either[x, y]): Either[y, x] = v match {
            case Left(x) => Right(x)
            case Right(y) => Left(y)
        }
        arr[Either[d, b], Either[b, d]](mirror) >>> left(f) >>> arr(mirror)
    }

    def op_+++[b, c, b_, c_](f: a[b, c])(g: a[b_, c_]): a[Either[b, b_], Either[c, c_]] = {
        left(f) >>> right(g)
    }

    def op_|||[b, c, d](f: a[b, d])(g: a[c, d]): a[Either[b, c], d] = {
        def utag[x](v: Either[x, x]): x = v match {
            case Left(x) => x
            case Right(y) => y
        }
        f +++ g >>> arr(utag)
    }

    private[ken] class Op_+++[b, c](f: a[b, c])(implicit i: ArrowChoice[a]) {
        def +++[b_, c_](g: a[b_, c_]): a[Either[b, b_], Either[c, c_]] = op_+++(f)(g)
    }
    implicit def +++[b, c](f: a[b, c])(implicit i: ArrowChoice[a]): Op_+++[b, c] = new Op_+++[b, c](f)

    private[ken] class Op_|||[b, d](f: a[b, d])(implicit i: ArrowChoice[a]) {
        def |||[c](g: a[c, d]): a[Either[b, c], d] = op_|||(f)(g)
    }
    implicit def |||[b, d](f: a[b, d])(implicit i: ArrowChoice[a]): Op_|||[b, d] = new Op_|||[b, d](f)

}

trait ArrowApply[a[_, _]] extends Arrow[a] {
    def app[b, c]: a[(a[b, c], b), c]
}

trait ArrowLoop[a[_, _]] extends Arrow[a] {
    def loop[b, c, d](f: a[(b, d), (c, d)]): a[b, c]
}


object Arrow extends ArrowInstance {
    def apply[a[_, _]](implicit i: Arrow[a]): Arrow[a] = i
}

object ArrowZero {
    def apply[a[_, _]](implicit i: ArrowZero[a]): ArrowZero[a] = i
}

object ArrowPlus {
    def apply[a[_, _]](implicit i: ArrowPlus[a]): ArrowPlus[a] = i
}

object ArrowChoice {
    def apply[a[_, _]](implicit i: ArrowChoice[a]): ArrowChoice[a] = i
}

object ArrowApply {
    def apply[a[_, _]](implicit i: ArrowApply[a]): ArrowApply[a] = i
}

object ArrowLoop {
    def apply[a[_, _]](implicit i: ArrowLoop[a]): ArrowLoop[a] = i
}


trait ArrowInstance {
    /*implicit*/ object ofFunction1 extends ArrowChoice[Function1] with ArrowApply[Function1] {
        private[this] implicit val i = this
        // Category
        private[this] type cat[a, b] = Function1[a, b]
        override def cid[a]: cat[a, a] = ken.id[a]
        override def op_<<<[a, b, c](f: cat[b, c])(g: cat[a, b]): cat[a, c] = f.compose(g)
        // Arrow
        private[this] type a[a, b] = Function1[a, b]
        override def arr[b, c](f: b => c): a[b, c] = f
        override def first[b, c, d](f: a[b, c]): a[(b, d), (c, d)] = f *** ken.id[d]
        override def second[b, c, d](f: a[b, c]): a[(d, b), (d, c)] = ken.id[d]_ *** f
        override def op_***[b, c, b_, c_](f: a[b, c])(g: a[b_, c_]): a[(b, b_), (c, c_)] = { case (x, y) => (f(x), g(y)) }
        // ArrowChoice
        override def left[b, c, d](f: a[b, c]): a[Either[b, d], Either[c, d]] = f +++ ken.id[d]
        override def right[b, c, d](f: a[b, c]): a[Either[d, b], Either[d, c]] = ken.id[d]_ +++ f
        override def op_+++[b, c, b_, c_](f: a[b, c])(g: a[b_, c_]): a[Either[b, b_], Either[c, c_]] = ((x: b) => Left(f(x)).of[c, c_]) ||| ((x: b_) => Right(g(x)).of[c, c_])
        override def op_|||[b, c, d](f: a[b, d])(g: a[c, d]): a[Either[b, c], d] = Either.either(f)(g)
        // ArrowApply
        override def app[b, c]: a[(a[b, c], b), c] = { case (f, x) => f(x) }
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
    implicit def ofKleisliArrow[m[_]](implicit i: Monad[m]): Arrow[({type a[a, b] = a => m[b]})#a] = new OfKleisliArrow[m]

    private[this] class OfKleisliArrowZero[m[_]](implicit i: MonadPlus[m]) extends OfKleisliArrow[m] with ArrowZero[({type a[a, b] = a => m[b]})#a] {
        override def zeroArrow[b, c]: a[b, c] = _ => mzero
    }
    implicit def ofKleisliArrowZero[m[_]](implicit i: MonadPlus[m]): ArrowZero[({type a[a, b] = a => m[b]})#a] = new OfKleisliArrowZero[m]

    implicit def ofKleisliArrowPlus[m[_]](implicit i: MonadPlus[m]): ArrowPlus[({type a[a, b] = a => m[b]})#a] = new OfKleisliArrowZero[m] with ArrowPlus[({type a[a, b] = a => m[b]})#a] {
        override def op_<+>[b, c](f: a[b, c])(g: => a[b, c]): a[b, c] = { x => f(x) _mplus_ g(x) }
    }
*/
}
