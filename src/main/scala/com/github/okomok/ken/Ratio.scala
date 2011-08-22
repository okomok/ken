

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


import scala.annotation.tailrec


sealed class Ratio[a](val numerator: a, val denominator:a) {
    override def equals(that: Any): Boolean = that match {
        case that: Ratio[_] => numerator == that.numerator && denominator == that.denominator
        case _ => false
    }
    override def toString: String = "Ratio(" + numerator + "," + denominator + ")"
    override def hashCode: Int = (numerator, denominator).hashCode
}


object Ratio {
    def apply[a](x: a, y: a)(implicit i: Integral[a]): Ratio[a] = {
        import i._
        reduce(x * signum(y))(abs(y))
    }

    def unapply[a](r: Ratio[a]): Option[(a, a)] = Some((r.numerator, r.denominator))

    private final val ratioPrec = 7
    private final val ratioPrec1 = 8

    val infinity: Rational = new Ratio(1, 0)
    val notANumber: Rational = new Ratio(0, 0)

    private def reduce[a](x: a)(y: a)(implicit i: Integral[a]): Ratio[a] = {
        import i._
        if (y === 0) {
            error("Ratio: zero denominator")
        } else {
            val d = gcd(x)(y)
            new Ratio(x _quot_ d, y _quot_ d)
        }
    }
/*
    def op_%%[a](x: a)(y: a)(implicit i: Integral[a]): Ratio[a] = Ratio(x, y)

    sealed class Op_%%[a](x: a) {
        def %%(y: a)(implicit i: Integral[a]): Ratio[a] = op_%%(x)(y)
    }
    implicit def %%[a](x: a): Op_%%[a] = new Op_%%(x)
*/
    def numerator[a](r: Ratio[a]): a = r.numerator
    def denominator[a](r: Ratio[a]): a = r.denominator

    private def gcd[a](x: a)(y: a)(implicit i: Integral[a]): a = {
        import i._
        @tailrec def gcd_(a: a)(b: a): a = if (b === 0) a else gcd_(b)(a _rem_ b)
        gcd_(abs(x))(abs(y))
    }
}
