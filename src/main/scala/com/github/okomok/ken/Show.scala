

// Copyright Shunsuke Sogame 2011.
//
// Copyright 2004, The University Court of the University of Glasgow.
// All rights reserved.
//
// Copyright (c) 2002 Simon Peyton Jones
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


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
}


sealed trait ShowInstance { this: Show.type =>
    implicit val _ofChar: Show[Char] = Char
    implicit val _ofDouble: Show[Double] = Double
    implicit val _ofFloat: Show[Float] = Float
    implicit val _ofInt: Show[Int] = Int
    implicit val _ofInteger: Show[Integer] = _Integer
    implicit val _ofUnit: Show[Unit] = Unit

    implicit def _ofAny[a]: Show[a] = new Show[a] {
        override val showsPrec: Int => a => ShowS = _ => x => showString(x.toString)
    }

    implicit def _ofList[z](implicit i: Show[z]): Show[List[z]] = new Show[List[z]] {
        private[this] type a = List[z]
        override val showsPrec: Int => a => ShowS = _ => x => i.showList(x)
    }

    // TODO
}
