

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


import Show._


trait Show[a] extends TypeClass0[a] {
    final def asShow: Show[apply] = this

    // Core
    //
    def showsPrec(n: Int)(x: a): ShowS
    def show(x: a): String_ = shows(x)("")
    def showList(ls: List[a]): ShowS = { s => showList__(shows)(ls)(s) }

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

    def shows(x: a): ShowS = showsPrec(0)(x)
}


trait ShowProxy[a] extends Show[a] with Proxy {
    override def self: Show[a]

    override def showsPrec(n: Int)(x: a): ShowS = self.showsPrec(n)(x)
    override def show(x: a): String_ = self.show(x)
    override def showList(ls: List[a]): ShowS = self.showList(ls)

    override def shows(x: a): ShowS = self.shows(x)
}


object Show extends ShowInstance {
    def apply[a](implicit i: Show[a]): Show[a] = i

    type ShowS = Function1[String_, String_]

    val showChar: Char => ShowS = List.op_!::

    val showString: String_ => ShowS = List.op_!:::

    val showParen: Bool => ShowS => ShowS = b => p => if (b) showChar('(') compose p compose showChar(')') else p

    val showSpace: ShowS = { xs => ' ' :: xs }
}


trait ShowInstance { this: Show.type =>
    implicit def ofAny[a]: Show[a] = new Show[a] {
        override def showsPrec(n: Int)(x: a): ShowS = showString(x.toString)
    }

    implicit def ofList[z](implicit i: Show[z]): Show[List[z]] = new Show[List[z]] {
        private[this] type a = List[z]
        override def showsPrec(n: Int)(x: a): ShowS = i.showList(x)
    }

    // TODO
}
