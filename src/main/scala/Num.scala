

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Num[a] {
    def op_+(x: a)(y: a): a
    def op_-(x: a)(y: a): a = op_+(x)(negate(y))
    def op_*(x: a)(y: a): a
    def negate(x: a): a = op_-(fromInteger(0))(x)
    def abs(x: a): a
    def signum(x: a): a
    def fromInteger(n: Int): a
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
        override def op_+(x: a)(y: a): a = i.plus(x, y)
        override def op_-(x: a)(y: a): a = i.minus(x, y)
        override def op_*(x: a)(y: a): a = i.times(x, y)
        override def negate(x: a): a = i.negate(x)
        override def abs(x: a): a = i.abs(x)
        override def signum(x: a): a = fromInteger(i.signum(x))
        override def fromInteger(n: Int): a = i.fromInt(n)
    }
}
