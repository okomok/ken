

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


// Following Java protocol, you should implement `toString` instead of `Show`.
// By the way, `showList` isn't used. (hard-coded in `List.toString`)

trait Show[a] extends Typeclass0[a] {
    final val asShow: Show[apply0] = this

    // Core
    //
    type showsPrec = Int => a => ShowS
    def showsPrec: showsPrec = _ => x => s => show(x) ++: s

    type show = a => String
    def show: show = x => shows(x)("")

    type showList = List[a] => ShowS
    final def showList: showList = ls => s => showList__(shows)(ls)(s)

    // Extra
    //
    type shows = a => ShowS
    def shows: shows = x => showsPrec(0)(x)

    private[this] def showList__(showx: a => ShowS)(xs: List[a]): ShowS = s => {
        xs match {
            case Nil => "Nil" ++: s
            case x :: xs => {
                def showl(ys: List[a]): String = ys match {
                    case Nil => ')' :: s
                    case y :: ys => ',' :: showx(y)(showl(ys.!))
                }
                "List(" ++: showx(x)(showl(xs.!))
            }
        }
    }
}


trait ShowProxy[a] extends Show[a] {
    def selfShow: Show[a]

    override val showsPrec: showsPrec = selfShow.showsPrec
    override val show: show = selfShow.show

    override val shows: shows = selfShow.shows
}


object Show extends ShowInstance with ShowShortcut {
    def apply[a <: Kind.Function0](implicit i: Show[a#apply0]): Show[a#apply0] = i

    val showChar: Char => ShowS = List.op_!::
    val showString: String => ShowS = List.op_!++:
    val showParen: Bool => ShowS => ShowS = b => p => if (b) showChar('(') `.` p `.` showChar(')') else p
    val showSpace: ShowS = xs => ' ' :: xs

    trait Of[a] extends Show[a] {
        override val showsPrec: showsPrec = _ => a => showString(a.toString)
    }
}


sealed trait ShowInstance { this: Show.type =>
/*
    implicit val ofBool: Show[Bool] = _Bool
    implicit val ofChar: Show[Char] = Char
    implicit val ofDouble: Show[Double] = Double
    implicit val ofFloat: Show[Float] = Float
    implicit val ofInt: Show[Int] = Int
    implicit val ofInteger: Show[Integer] = _Integer
    implicit val ofUnit: Show[Unit] = Unit
    implicit val ofNothing: Show[Nothing] = of[Nothing]
*/
    implicit def of[a]: Show[a] = new Of[a] {}
}


sealed trait ShowShortcut { this: Show.type =>
    def showsPrec[a](x: Int)(s: a)(implicit i: Show[a]): ShowS = i.showsPrec(x)(s)
    def show[a](s: a)(implicit i: Show[a]): String = i.show(s)
    def showList[a](ls: List[a])(implicit i: Show[a]): ShowS = i.showList(ls)
    def shows[a](x: a)(implicit i: Show[a]): ShowS = i.shows(x)
}
