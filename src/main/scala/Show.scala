

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


/*
import Show._


trait Show[a] {
//    def showPrec(n: Int)(x: a): ShowS
    def show(x: a): String = shows(x)("")
//    def showList(ls: List[a]): ShowS = showList__(shows)(ls)(s)
}


object Show extends ShowOp with ShowInstance


trait ShowOp {
    type ShowS = Function1[String, String]

    def show[a](x: a)(implicit i: Show[a]): String = i.show(x)
}


trait ShowInstance {
    implicit object ofAny[a](x: a): Show[a] = new Show[a] {
        override def showPrec(n: Int)(x: a): ShowS = showString(x.toString)
        override def show(x: a): String = x.toString
    }
}
*/
