

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Num[a] { outer =>
    def op_+(x: a)(y: a): a
    def op_-(x: a)(y: a): a = op_+(x)(negate(y))
    def op_*(x: a)(y: a): a
    def negate(x: a): a = op_-(fromInt(0))(x)
    def abs(x: a): a
    def signum(x: a): a
    def fromInt(n: Int): a
    final def subtract(x: a)(y: a): a = y - x

    implicit def method(x: a): NumMethod[a] = new NumMethod[a] {
        override val klass = outer
        override val callee = x
    }
}

trait NumMethod[a] extends Method {
    override def klass: Num[a]
    override def callee: a
    final def +(y: a): a = klass.op_+(callee)(y)
    final def -(y: a): a = klass.op_-(callee)(y)
    final def *(y: a): a = klass.op_*(callee)(y)
}

trait NumProxy[a] extends Num[a] with Proxy {
    override def self: Num[a]
    override def op_+(x: a)(y: a): a = self.op_+(x)(y)
    override def op_-(x: a)(y: a): a = self.op_-(x)(y)
    override def op_*(x: a)(y: a): a = self.op_*(x)(y)
    override def negate(x: a): a = self.negate(x)
    override def abs(x: a): a = self.abs(x)
    override def signum(x: a): a = self.signum(x)
    override def fromInt(n: Int): a = self.fromInt(n)
}


object Num extends NumInstance {
    def apply[a](implicit i: Num[a]): Num[a] = i
}

trait NumInstance {
    implicit def ofNumeric[a](implicit i: Numeric[a]): Num[a] = new Num[a] {
        override def op_+(x: a)(y: a): a = i.plus(x, y)
        override def op_-(x: a)(y: a): a = i.minus(x, y)
        override def op_*(x: a)(y: a): a = i.times(x, y)
        override def negate(x: a): a = i.negate(x)
        override def abs(x: a): a = i.abs(x)
        override def signum(x: a): a = fromInt(i.signum(x))
        override def fromInt(n: Int): a = i.fromInt(n)
    }
}
