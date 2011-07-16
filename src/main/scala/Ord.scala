

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Ord[a] extends Klass { outer =>
    def compare(x: a)(y: a): Ordering = if (x == y) EQ else if (op_<=(x)(y)) LT else GT
    def op_<(x: a)(y: a): Boolean = compare(x)(y) match { case LT => true; case _ => false }
    def op_<=(x: a)(y: a): Boolean = compare(x)(y) match { case GT => false; case _ => true }
    def op_>(x: a)(y: a): Boolean = compare(x)(y) match { case GT => true; case _ => false }
    def op_>=(x: a)(y: a): Boolean = compare(x)(y) match { case LT => false; case _ => true }
    def max(x: a)(y: a): a = if (op_<=(x)(y)) y else x
    def min(x: a)(y: a): a = if (op_<=(x)(y)) x else y

    implicit def method(x: a): OrdMethod[a] = new OrdMethod[a] {
        override def klass = outer
        override def callee = x
    }
}


trait OrdMethod[a] extends Method {
    override def klass: Ord[a]
    override def callee: a
    final def <(y: a): Boolean = klass.op_<(callee)(y)
    final def <=(y: a): Boolean = klass.op_<=(callee)(y)
    final def >(y: a): Boolean = klass.op_>(callee)(y)
    final def >=(y: a): Boolean = klass.op_>=(callee)(y)
}


trait OrdProxy[a] extends Ord[a] with Proxy {
    override def self: Ord[a]
    override def compare(x: a)(y: a): Ordering = self.compare(x)(y)
    override def op_<(x: a)(y: a): Boolean = self.op_<(x)(y)
    override def op_<=(x: a)(y: a): Boolean = self.op_<=(x)(y)
    override def op_>(x: a)(y: a): Boolean = self.op_>(x)(y)
    override def op_>=(x: a)(y: a): Boolean = self.op_>=(x)(y)
    override def max(x: a)(y: a): a = self.max(x)(y)
    override def min(x: a)(y: a): a = self.min(x)(y)
}


object Ord {
    def apply[a](implicit i: Ord[a]): Ord[a] = i

    implicit def ofOrdering[a](implicit i: scala.Ordering[a]): Ord[a] = new Ord[a] {
        override def compare(x: a)(y: a): Ordering = i.compare(x, y) match {
            case 0 => EQ
            case s if s < 0 => LT
            case s if s > 0 => GT
        }
        override def op_<(x: a)(y: a): Boolean = i.lt(x, y)
        override def op_<=(x: a)(y: a): Boolean = i.lteq(x, y)
        override def op_>(x: a)(y: a): Boolean = i.gt(x, y)
        override def op_>=(x: a)(y: a): Boolean = i.gteq(x, y)
        override def max(x: a)(y: a): a = i.max(x, y)
        override def min(x: a)(y: a): a = i.min(x, y)
    }
}
