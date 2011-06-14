

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Num[a] {
    def op_+ : a => a => a
    def op_- : a => a => a = { x => y => op_+(x)(negate(y)) }
    def op_* : a => a => a
    def negate: a => a = { x => op_-(fromInteger(0))(x) }
    def abs: a => a
    def signum: a => a
    def fromInteger: Int => a
}

object Num {
    def op_+[a](x: a)(y: a)(implicit i: Num[a]): a = i.op_+(x)(y)
    def op_-[a](x: a)(y: a)(implicit i: Num[a]): a = i.op_-(x)(y)
    def op_*[a](x: a)(y: a)(implicit i: Num[a]): a = i.op_*(x)(y)
    def negate[a](x: a)(implicit i: Num[a]): a = i.negate(x)
    def abs[a](x: a)(implicit i: Num[a]): a = i.abs(x)
    def signum[a](x: a)(implicit i: Num[a]): a = i.signum(x)
    def fromInteger[a](n: Int, t: Type[a])(implicit i: Num[a]): a = i.fromInteger(n)
    def subtract[a](x: a)(y: a)(implicit i: Num[a]): a = y - x

    private[ken] class Op_-[a](x: a)(implicit i: Num[a]) {
        def -(y: a): a = op_-(x)(y)
    }
    implicit def -[a](x: a)(implicit i: Num[a]): Op_-[a] = new Op_-[a](x)

    private[ken] class Op_+[a](x: a)(implicit i: Num[a]) {
        def +(y: a): a = op_+(x)(y)
    }
    implicit def +[a](x: a)(implicit i: Num[a]): Op_+[a] = new Op_+[a](x)

    private[ken] class Op_*[a](x: a)(implicit i: Num[a]) {
        def *(y: a): a = op_*(x)(y)
    }
    implicit def *[a](x: a)(implicit i: Num[a]): Op_*[a] = new Op_*[a](x)

    implicit def instanceOfNumeric[a](implicit i: Numeric[a]): Num[a] = new Num[a] {
        override def op_+ : a => a => a = { x => y => i.plus(x, y) }
        override def op_- : a => a => a = { x => y => i.minus(x, y) }
        override def op_* : a => a => a = { x => y => i.times(x, y) }
        override def negate: a => a = i.negate
        override def abs: a => a = i.abs
        override def signum: a => a = { x => fromInteger(i.signum(x)) }
        override def fromInteger: Int => a = i.fromInt
    }
}
