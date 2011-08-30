

// Copyright Shunsuke Sogame 2011.
//
// (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package parsec


final case class SourcePos(name: SourceName, line: Line, column: Column) {
    override def toString = List.stringize(SourcePos.show(this))
}


object SourcePos extends Eq.Of[SourcePos] with Ord[SourcePos] with Show[SourcePos] {
    // Overrides
    //
    // Ord
    private type a = SourcePos
    override val compare: a => a => Ordering = { case SourcePos(n1, l1, c1) => { case SourcePos(n2, l2, c2) => {
        val i = Ord.ofScalaOrdering[(SourceName, Line, Column)]
        i.compare((n1, l1, c1))((n2, l2, c2))
    } } }
    // Show
    override val show: a => String_ = { case SourcePos(n, l, c) => showSourcePos(n, l, c) }

    private def showSourcePos(name: SourceName, line: Line, column: Column): String_ = {
        def showLineColumn: String_ = "(line " ::: ken.show(line) ::: ", column " ::: ken.show(column) ::: List.from(")")
        if (name == "") {
            showLineColumn
        } else {
            "\"" ::: name ::: "\" " ::: showLineColumn
        }
    }
}
