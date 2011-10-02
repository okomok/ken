

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


trait Show[-a] extends Typeclass0[a] {
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


trait ShowProxy[-a] extends Show[a] {
    def selfShow: Show[a]

    override val showsPrec: showsPrec = selfShow.showsPrec
    override val show: show = selfShow.show
    override val showList: showList = selfShow.showList

    override val shows: shows = selfShow.shows
}


object Show extends ShowInstance with ShowShortcut {
    def apply[a <: Kind.Function0](implicit i: Show[a#apply0]): Show[a#apply0] = i

    def deriving[nt <: Kind.Newtype0](implicit j: Newtype0[nt#apply0, nt#oldtype0, _], i: Show[nt#oldtype0]): Show[nt#apply0] = new Show[nt#apply0] {
        override val showsPrec: showsPrec = _ => x => show_prefix(x) `.` showChar('(') `.` i.shows(j.oldOf(x)) `.` showChar(')')
        //override val show: show = a => show_prefix(a) `.` showChar('(') `.` i.show(j.oldOf(a)) `.` showChar(')')
        //override val showList: showList = ls => show_prefix(a) `.` showChar('(') `.` i.showList(List.map((x: nt#apply0) => j.oldOf(x))(ls)) `.` showChar(')')

        //override val shows: shows = a => i.shows(j.oldOf(a))
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

    private[ken] val show_product: List[ShowS] => ShowS => ShowS = ss => prefix => {
        prefix `.` showChar('(') `.` List.foldr1((s: ShowS) => (r: Lazy[ShowS]) => s `.` showChar(',') `.` r)(ss) `.` showChar(')')
    }

    private[ken] val show_prefix: Any => ShowS = x => xs => List.takeWhile[Char](_ /== '(')(x.toString) ++: xs

    trait Deriving
}


private[ken] sealed trait ShowInstance0 { this: Show.type =>
    val ofAny: Show[Any] = new Default[Any] {}

    implicit def ofDefault[a]: Show[a] = ofAny // vs `ofNewtype0`
}

private[ken] sealed trait ShowInstance1 extends ShowInstance0 { this: Show.type =>

    // Primitives
    //
    implicit val ofBool: Show[Bool] = _Bool
    implicit val ofChar: Show[Char] = Char
    implicit val ofDouble: Show[Double] = Double
    implicit val ofFloat: Show[Float] = Float
    implicit val ofInt: Show[Int] = Int
    implicit val ofInteger: Show[Integer] = _Integer
    implicit val ofUnit: Show[Unit] = Unit

    implicit def ofNewtype0[nt, ot, ds <: Kind.MethodList](implicit j: Newtype0[nt, ot, ds], i: Show[ot], k: Kind.MethodList.Contains[ds, Real]): Show[nt] = deriving[Newtype0[nt, ot, _]]

    // Products
    //
    implicit def ofProduct1[a](implicit i1: Show[a]): Show[Product1[a] with Deriving] = new Show[Product1[a] with Deriving] {
        override val showsPrec: showsPrec = _ => x => show_product(List(i1.shows(x._1)))(show_prefix(x))
    }
    implicit def ofProduct2[a, b](implicit i1: Show[a], i2: Show[b]): Show[Product2[a, b] with Deriving] = new Show[Product2[a, b] with Deriving] {
        override val showsPrec: showsPrec = _ => x => show_product(List(i1.shows(x._1), i2.shows(x._2)))(show_prefix(x))
    }
    implicit def ofProduct3[a, b, c](implicit i1: Show[a], i2: Show[b], i3: Show[c]): Show[Product3[a, b, c] with Deriving] = new Show[Product3[a, b, c] with Deriving] {
        override val showsPrec: showsPrec = _ => x => show_product(List(i1.shows(x._1), i2.shows(x._2), i3.shows(x._3)))(show_prefix(x))
    }
    implicit def ofProduct4[a, b, c, d](implicit i1: Show[a], i2: Show[b], i3: Show[c], i4: Show[d]): Show[Product4[a, b, c, d] with Deriving] = new Show[Product4[a, b, c, d] with Deriving] {
        override val showsPrec: showsPrec = _ => x => show_product(List(i1.shows(x._1), i2.shows(x._2), i3.shows(x._3), i4.shows(x._4)))(show_prefix(x))
    }
    implicit def ofProduct5[a, b, c, d, e](implicit i1: Show[a], i2: Show[b], i3: Show[c], i4: Show[d], i5: Show[e]): Show[Product5[a, b, c, d, e] with Deriving] = new Show[Product5[a, b, c, d, e] with Deriving] {
        override val showsPrec: showsPrec = _ => x => show_product(List(i1.shows(x._1), i2.shows(x._2), i3.shows(x._3), i4.shows(x._4), i5.shows(x._5)))(show_prefix(x))
    }

    // Tuples
    //
    implicit def ofTuple1[a](implicit i1: Show[a]): Show[Tuple1[a]] = new Show[Tuple1[a]] {
        override val showsPrec: showsPrec = _ => x => show_tuple(List(i1.shows(x._1)))
    }
    implicit def ofTuple2[a, b](implicit i1: Show[a], i2: Show[b]): Show[Tuple2[a, b]] = new Show[Tuple2[a, b]] {
        override val showsPrec: showsPrec = _ => x => show_tuple(List(i1.shows(x._1), i2.shows(x._2)))
    }
    implicit def ofTuple3[a, b, c](implicit i1: Show[a], i2: Show[b], i3: Show[c]): Show[Tuple3[a, b, c]] = new Show[Tuple3[a, b, c]] {
        override val showsPrec: showsPrec = _ => x => show_tuple(List(i1.shows(x._1), i2.shows(x._2), i3.shows(x._3)))
    }
    implicit def ofTuple4[a, b, c, d](implicit i1: Show[a], i2: Show[b], i3: Show[c], i4: Show[d]): Show[Tuple4[a, b, c, d]] = new Show[Tuple4[a, b, c, d]] {
        override val showsPrec: showsPrec = _ => x => show_tuple(List(i1.shows(x._1), i2.shows(x._2), i3.shows(x._3), i4.shows(x._4)))
    }
    implicit def ofTuple5[a, b, c, d, e](implicit i1: Show[a], i2: Show[b], i3: Show[c], i4: Show[d], i5: Show[e]): Show[Tuple5[a, b, c, d, e]] = new Show[Tuple5[a, b, c, d, e]] {
        override val showsPrec: showsPrec = _ => x => show_tuple(List(i1.shows(x._1), i2.shows(x._2), i3.shows(x._3), i4.shows(x._4), i5.shows(x._5)))
    }
}

sealed trait ShowInstance extends ShowInstance1 { this: Show.type =>
    implicit val ofNothing: Show[Nothing] with HighPriority = new Show[Nothing] with HighPriority
}


sealed trait ShowShortcut { this: Show.type =>
    def showsPrec[a](x: Int)(s: a)(implicit i: Show[a]): ShowS = i.showsPrec(x)(s)
    def show[a](s: a)(implicit i: Show[a]): String = i.show(s)
    def showList[a](ls: List[a])(implicit i: Show[a]): ShowS = i.showList(ls)
    def shows[a](x: a)(implicit i: Show[a]): ShowS = i.shows(x)
}
