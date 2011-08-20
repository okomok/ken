

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


// Draft


final case class Ratio[a](numerator: a, denominator:a)

object Ratio {
    final val ratioPrec = 7
    final val ratioPrec1 = 8

    val infinity: Rational = Ratio(1, 0)
    val notANumber: Rational = Ratio(0, 0)

    def reduce[a](x: a)(y: a)(implicit i: Integral[a]): Ratio[a] = {
        import i._
        if (op_==(y)(0)) {
            error("Ratio.%: zero denominator")
        } else {
            val d = gcd(x)(y)
            Ratio(x _quot_ d, y _quot_ d)
        }
    }

    def op_%[a](x: a)(y: a)(implicit i: Integral[a]): Ratio[a] = {
        import i._
        reduce(x * signum(y))(abs(y))
    }

    sealed class Op_%[a](x: a) {
        def %(y: a)(implicit i: Integral[a]): Ratio[a] = op_%(x)(y)
    }
    implicit def %[a](x: a): Op_%[a] = new Op_%(x)

    def numerator[a](r: Ratio[a])(implicit i: Integral[a]): a = r.numerator
    def denominator[a](r: Ratio[a])(implicit i: Integral[a]): a = r.denominator

    def gcd[a](x: a)(y: a)(implicit i: Integral[a]): a = {
        import i._
        def gcd_(a: a)(b: a): a = {
            if (i.op_==(b)(0)) a else gcd_(b)(a _rem_ b)
        }
        gcd_(abs(x))(abs(y))
    }
}
