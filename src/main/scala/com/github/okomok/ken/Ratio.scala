

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


import scala.annotation.tailrec


sealed class Ratio[a](val numerator: a, val denominator:a) extends Kind.constThis {
    override def equals(that: Any): Boolean = that match {
        case that: Ratio[_] => (numerator == that.numerator) && (denominator == that.denominator)
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

    implicit def _asRealFrac[z](implicit i: Integral[z]): RealFrac[Ratio[z]] = new RealFrac[Ratio[z]] {
        private[this] type a = Ratio[z]
        // Num
        override val op_+ : a => a => a = { case Ratio(x, y) => { case Ratio(x_, y_) => reduce(i.op_+(i.op_*(x)(y_))(i.op_*(x_)(y)))(i.op_*(y)(y_)) } }
        override val op_- : a => a => a = { case Ratio(x, y) => { case Ratio(x_, y_) => reduce(i.op_-(i.op_*(x)(y_))(i.op_*(x_)(y)))(i.op_*(y)(y_)) } }
        override val op_* : a => a => a = { case Ratio(x, y) => { case Ratio(x_, y_) => reduce(i.op_*(x)(x_))(i.op_*(y)(y_)) } }
        override val negate: a => a = { case Ratio(x, y) => new Ratio(i.negate(x), y) }
        override val abs: a => a = { case Ratio(x, y) => new Ratio(i.abs(x), y) }
        override val signum: a => a = { case Ratio(x, _) => new Ratio(i.signum(x), i.fromInt(1))  }
        override val fromInteger: Integer => a = x => new Ratio(i.fromInteger(x), i.fromInt(1))
        // Fractional
        override val op_/ : a => a => a = { case Ratio(x, y) => { case Ratio(x_, y_) => Ratio(i.op_*(x)(y), i.op_*(x_)(y_))  } }
        override val recip: a => a = { case Ratio(x, y) => Ratio(y, x) }
        override val fromRational: Rational => a = { case Ratio(x, y) => new Ratio(i.fromInteger(x), i.fromInteger(y)) }
        // Real
        override val toRational: a => Rational = { case Ratio(x, y) => new Ratio(i.toInteger(x), i.toInteger(y)) }
        // RealFrac
        override def properFraction[b](r: a)(implicit j : Integral[b]): (b, a) = r match {
            case Ratio(x, y) => {
                val (q, r) = i.quotRem(x)(y)
                (j.fromInteger(i.toInteger(q)), new Ratio(r, y))
            }
        }
    }

    implicit def _asEnum[z](implicit i: Integral[z]): Enum[Ratio[z]] = new Enum[Ratio[z]] {
        private[this] val j = _asRealFrac[z]
        private[this] type a = Ratio[z]
        override val succ: a => a = x => j.op_+(x)(j.fromInt(1))
        override val pred: a => a = x => j.op_-(x)(j.fromInt(1))
        override val toEnum: Int => a = n => new Ratio(i.fromIntegral(n), i.fromInt(1))
        override val fromEnum: a => Int = Num[Kind.const[Int]].fromInteger compose j.truncate[Integer]
    }
}
