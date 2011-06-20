

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Arrow[a[_, _]] extends Category[a] {
    private[this] implicit val i = this
    import Arrow.{<<<,>>>,***}

    def arr[b, c](f: b => c): a[b, c]
    def first[b, c, d](f: a[b, c]): a[(b, d), (c, d)]
    def second[b, c, d](f: a[b, c]): a[(d, b), (d, c)] = {
        def swap[x, y](v: (x, y)): (y, x) = (v._2, v._1)
        arr(swap[d, b]) >>> first(f) >>> arr(swap[c, d])
    }

    def op_***[b, c, b_, c_](f: a[b, c])(g: a[b_, c_]): a[(b, b_), (c, c_)] = first(f) >>> second(g)
    def op_&&&[b, c, c_](f: a[b, c])(g: a[b, c_]): a[b, (c, c_)] = arr((b: b) => (b, b)) >>> f *** g

    override def id[a_]: a[a_, a_] = arr(ken.id[a_])
}


trait ArrowZero[a[_, _]] extends Arrow[a] {
    def zeroArrow[b, c]: a[b, c]
}


trait ArrowPlus[a[_, _]] extends ArrowZero[a] {
    def op_<+>[b, c](f: a[b, c])(g: => a[b, c]): a[b, c]
}


trait ArrowChoice[a[_, _]] extends Arrow[a] {
    private[this] implicit val i = this
    import Arrow.{>>>, +++}

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
}


object Arrow extends CategoryOp {

// Arrow
    def arr[a[_, _], b, c](f: b => c)(implicit i: Arrow[a]): a[b, c] = i.arr(f)
    def first[a[_, _], b, c, d](f: a[b, c])(implicit i: Arrow[a]): a[(b, d), (c, d)] = i.first(f)
    def second[a[_, _], b, c, d](f: a[b, c])(implicit i: Arrow[a]): a[(d, b), (d, c)] = i.second(f)
    def op_***[a[_, _], b, c, b_, c_](f: a[b, c])(g: a[b_, c_])(implicit i: Arrow[a]): a[(b, b_), (c, c_)] = i.op_***(f)(g)
    def op_&&&[a[_, _], b, c, c_](f: a[b, c])(g: a[b, c_])(implicit i: Arrow[a]): a[b, (c, c_)] = i.op_&&&(f)(g)

    private[ken] class Op_***[a[_, _], b, c](f: a[b, c])(implicit i: Arrow[a]) {
        def ***[b_, c_](g: a[b_, c_]): a[(b, b_), (c, c_)] = op_***(f)(g)
    }
    implicit def ***[a[_, _], b, c](f: a[b, c])(implicit i: Arrow[a]): Op_***[a, b, c] = new Op_***[a, b, c](f)

    private[ken] class Op_&&&[a[_, _], b, c](f: a[b, c])(implicit i: Arrow[a]) {
        def &&&[c_](g: a[b, c_]): a[b, (c, c_)] = op_&&&(f)(g)
    }
    implicit def &&&[a[_, _], b, c](f: a[b, c])(implicit i: Arrow[a]): Op_&&&[a, b, c] = new Op_&&&[a, b, c](f)

    def returnA[a[_, _], b](implicit i: Arrow[a]): a[b, b] = arr(ken.id[b])

    def op_^>>[a[_, _], b, c, d](f: b => c)(a: a[c, d])(implicit i: Arrow[a]): a[b, d] = arr(f) >>> a
    def op_>>^[a[_, _], b, c, d](a: a[b, c])(f: c => d)(implicit i: Arrow[a]): a[b, d] = a >>> arr(f)
    def op_<<^[a[_, _], b, c, d](a: a[c, d])(f: b => c)(implicit i: Arrow[a]): a[b, d] = a <<< arr(f)
    def op_^<<[a[_, _], b, c, d](f: c => d)(a: a[b, c])(implicit i: Arrow[a]): a[b, d] = arr(f) <<< a

    private[ken] class Op_^>>[a[_, _], b, c](f: b => c)(implicit i: Arrow[a]) {
        def ^>>[d](a: a[c, d]): a[b, d] = op_^>>(f)(a)
    }
    implicit def ^>>[a[_, _], b, c](f: b => c)(implicit i: Arrow[a]): Op_^>>[a, b, c] = new Op_^>>[a, b, c](f)

    private[ken] class Op_>>^[a[_, _], b, c](a: a[b, c])(implicit i: Arrow[a]) {
        def >>^[d](f: c => d): a[b, d] = op_>>^(a)(f)
    }
    implicit def >>^[a[_, _], b, c](a: a[b, c])(implicit i: Arrow[a]): Op_>>^[a, b, c] = new Op_>>^[a, b, c](a)

    private[ken] class Op_<<^[a[_, _], c, d](a: a[c, d])(implicit i: Arrow[a]) {
        def <<^[b](f: b => c): a[b, d] = op_<<^(a)(f)
    }
    implicit def <<^[a[_, _], c, d](a: a[c, d])(implicit i: Arrow[a]): Op_<<^[a, c, d] = new Op_<<^[a, c, d](a)

    private[ken] class Op_^<<[a[_, _], c, d](f: c => d)(implicit i: Arrow[a]) {
        def ^<<[b](a: a[b, c]): a[b, d] = op_^<<(f)(a)
    }
    implicit def ^<<[a[_, _], c, d](f: c => d)(implicit i: Arrow[a]): Op_^<<[a, c, d] = new Op_^<<[a, c, d](f)

    class InstanceOfFunction1 extends Category.InstanceOfFunction1 with Arrow[Function1] {
        private[this] implicit val i = this
        private[this] type a[a, b] = Function1[a, b]
        override def arr[b, c](f: b => c): a[b, c] = f
        override def first[b, c, d](f: a[b, c]): a[(b, d), (c, d)] = f *** ken.id[d]
        override def second[b, c, d](f: a[b, c]): a[(d, b), (d, c)] = ken.id[d]_ *** f
        override def op_***[b, c, b_, c_](f: a[b, c])(g: a[b_, c_]): a[(b, b_), (c, c_)] = { case (x, y) => (f(x), g(y)) }
    }
    /*implicit*/ val instanceOfFunction1 = new InstanceOfFunction1

// ArrowZero
    def zeroArrow[a[_, _], b, c](implicit i: ArrowZero[a]): a[b, c] = i.zeroArrow

// ArrowPlus
    def op_<+>[a[_, _], b, c](f: a[b, c])(g: => a[b, c])(implicit i: ArrowPlus[a]): a[b, c] = i.op_<+>(f)(g)

    private[ken] class Op_<+>[a[_, _], b, c](f: a[b, c])(implicit i: ArrowPlus[a]) {
        def <+>(g: => a[b, c]): a[b, c] = op_<+>(f)(g)
    }
    implicit def <+>[a[_, _], b, c](f: a[b, c])(implicit i: ArrowPlus[a]): Op_<+>[a, b, c] = new Op_<+>[a, b, c](f)

// ArrowChoice
    def left[a[_, _], b, c, d](f: a[b, c])(implicit i: ArrowChoice[a]): a[Either[b, d], Either[c, d]] = i.left(f)
    def right[a[_, _], b, c, d](f: a[b, c])(implicit i: ArrowChoice[a]): a[Either[d, b], Either[d, c]] = i.right(f)
    def op_+++[a[_, _], b, c, b_, c_](f: a[b, c])(g: a[b_, c_])(implicit i: ArrowChoice[a]): a[Either[b, b_], Either[c, c_]] = i.op_+++(f)(g)
    def op_|||[a[_, _], b, c, d](f: a[b, d])(g: a[c, d])(implicit i: ArrowChoice[a]): a[Either[b, c], d] = i.op_|||(f)(g)

    private[ken] class Op_+++[a[_, _], b, c](f: a[b, c])(implicit i: ArrowChoice[a]) {
        def +++[b_, c_](g: a[b_, c_]): a[Either[b, b_], Either[c, c_]] = op_+++(f)(g)
    }
    implicit def +++[a[_, _], b, c](f: a[b, c])(implicit i: ArrowChoice[a]): Op_+++[a, b, c] = new Op_+++[a, b, c](f)

    private[ken] class Op_|||[a[_, _], b, d](f: a[b, d])(implicit i: ArrowChoice[a]) {
        def |||[c](g: a[c, d]): a[Either[b, c], d] = op_|||(f)(g)
    }
    implicit def |||[a[_, _], b, d](f: a[b, d])(implicit i: ArrowChoice[a]): Op_|||[a, b, d] = new Op_|||[a, b, d](f)
}
