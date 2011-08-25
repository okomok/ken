

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


import Show._


trait Show[a] extends Typeclass0[a] {
    final val asShow: Show[apply0] = this

    // Core
    //
    def showsPrec: Int => a => ShowS
    def show: a => String_ = x => shows(x)("")
    def showList: List[a] => ShowS = ls => s => showList__(shows)(ls)(s)

    // Extra
    //
    final def showList__(showx: a => ShowS)(xs: List[a]): ShowS = { s =>
        xs match {
            case Nil => "Nil" ::: s
            case x :: xs => {
                def showl(ys: List[a]): String_ = ys match {
                    case Nil => ')' :: s
                    case y :: ys => ',' :: showx(y)(showl(ys.!))
                }
                "List(" ::: showx(x)(showl(xs.!))
            }
        }
    }

    def shows: a => ShowS = x => showsPrec(0)(x)
}


trait ShowProxy[a] extends Show[a] {
    def selfShow: Show[a]

    override val showsPrec: Int => a => ShowS = selfShow.showsPrec
    override val show: a => String_ =  selfShow.show
    override val showList: List[a] => ShowS = selfShow.showList

    override val shows: a => ShowS = selfShow.shows
}


object Show extends ShowInstance {
    def apply[a](implicit i: Show[a]): Show[a] = i

    val showChar: Char => ShowS = List.op_!::

    val showString: String_ => ShowS = List.op_!:::

    val showParen: Bool => ShowS => ShowS = b => p => if (b) showChar('(') compose p compose showChar(')') else p

    val showSpace: ShowS = { xs => ' ' :: xs }

    private[ken] val showSignedInt: Int => Int => ShowS = p => n => r => {
        if (n < 0 && p > 6) {
            '(' :: itos(n)(')' :: r)
        } else {
            itos(n)(r)
        }
    }

    private[ken] val itos: Int => String_ => String_ = n => cs => {
        n.toString ::: cs
    }
}


sealed trait ShowInstance { this: Show.type =>
    implicit def _ofAny[a]: Show[a] = new Show[a] {
        override val showsPrec: Int => a => ShowS = _ => x => showString(x.toString)
    }

    implicit def _ofList[z](implicit i: Show[z]): Show[List[z]] = new Show[List[z]] {
        private[this] type a = List[z]
        override val showsPrec: Int => a => ShowS = _ => x => i.showList(x)
    }

    implicit val _ofInt: Show[Int] = Int

    // TODO
}
