

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
    type showsPrec = Int => a => ShowS
    def showsPrec: showsPrec = _ => x => s => show(x) ++: s

    type show = a => String
    def show: show = x => shows(x)("")

    type showList = List[a] => ShowS
    def showList: showList = ls => s => showList__(shows)(ls)(s)

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
    override val showList: showList = selfShow.showList

    override val shows: shows = selfShow.shows
}


object Show extends ShowInstance with ShowShortcut {
    def apply[a <: Kind.Function0](implicit i: Show[a#apply0]): Show[a#apply0] = i

    def deriving[nt <: Kind.Newtype0](implicit j: Newtype0[nt#apply0, nt#oldtype0, _], i: Show[nt#oldtype0]): Show[nt#apply0] = new Show[nt#apply0] {
        override val showsPrec: showsPrec = n => a => i.showsPrec(n)(j.oldOf(a))
        override val show: show = a => i.show(j.oldOf(a))
        override val showList: showList = ls => i.showList(List.map((x: nt#apply0) => j.oldOf(x))(ls))

        override val shows: shows = a => i.shows(j.oldOf(a))
    }

    def weak[nt <: Kind.Newtype0](implicit j: Newtype0[nt#apply0, nt#oldtype0, _], i: Show[nt#apply0]): Show[nt#oldtype0] = deriving[Kind.coNewtype0[nt]](j.coNewtype, i)

    val showChar: Char => ShowS = List.op_!::
    val showString: String => ShowS = List.op_!++:
    val showParen: Bool => ShowS => ShowS = b => p => if (b) showChar('(') `.` p `.` showChar(')') else p
    val showSpace: ShowS = xs => ' ' :: xs

    trait Default[a] extends Show[a] {
        override val showsPrec: showsPrec = _ => a => showString(a.toString)
    }

    private[ken] val show_tuple: List[ShowS] => ShowS = ss => {
        showChar('(') `.` List.foldr1((s: ShowS) => (r: Lazy[ShowS]) => s `.` showChar(',') `.` r)(ss) `.` showChar(')')
    }
}


sealed trait ShowInstance { this: Show.type =>
    implicit val ofBool: Show[Bool] = _Bool
    implicit val ofChar: Show[Char] = Char
    implicit val ofDouble: Show[Double] = Double
    implicit val ofFloat: Show[Float] = Float
    implicit val ofInt: Show[Int] = Int
    implicit val ofInteger: Show[Integer] = _Integer
    implicit val ofUnit: Show[Unit] = Unit

    implicit def ofDefault[a]: Show[a] = new Default[a] {}

    implicit def ofTuple2[a, b](implicit i1: Show[a], i2: Show[b]): Show[(a, b)] = new Show[(a, b)] {
        override val showsPrec: showsPrec = _ => { case (a, b) => show_tuple(List(i1.shows(a), i2.shows(b))) }
    }

    implicit def ofTuple3[a, b, c](implicit i1: Show[a], i2: Show[b], i3: Show[c]): Show[(a, b, c)] = new Show[(a, b, c)] {
        override val showsPrec: showsPrec = _ => { case (a, b, c) => show_tuple(List(i1.shows(a), i2.shows(b), i3.shows(c))) }
    }

    implicit def ofTuple4[a, b, c, d](implicit i1: Show[a], i2: Show[b], i3: Show[c], i4: Show[d]): Show[(a, b, c, d)] = new Show[(a, b, c, d)] {
        override val showsPrec: showsPrec = _ => { case (a, b, c, d) => show_tuple(List(i1.shows(a), i2.shows(b), i3.shows(c), i4.shows(d))) }
    }

    implicit def ofTuple5[a, b, c, d, e](implicit i1: Show[a], i2: Show[b], i3: Show[c], i4: Show[d], i5: Show[e]): Show[(a, b, c, d, e)] = new Show[(a, b, c, d, e)] {
        override val showsPrec: showsPrec = _ => { case (a, b, c, d, e) => show_tuple(List(i1.shows(a), i2.shows(b), i3.shows(c), i4.shows(d), i5.shows(e))) }
    }
}


sealed trait ShowShortcut { this: Show.type =>
    def showsPrec[a](x: Int)(s: a)(implicit i: Show[a]): ShowS = i.showsPrec(x)(s)
    def show[a](s: a)(implicit i: Show[a]): String = i.show(s)
    def showList[a](ls: List[a])(implicit i: Show[a]): ShowS = i.showList(ls)
    def shows[a](x: a)(implicit i: Show[a]): ShowS = i.shows(x)
}
