

// Copyright Shunsuke Sogame 2011.
//
// (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package parsec


private[ken] trait _Pos[n[+_]] { this: _Parsec[n] =>
    // Source positions
    //
    type SourceName = String
    type Line = Int
    type Column = Int

    // SourcePos
    //
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
    }

    val newPos: SourceName => Line => Column => SourcePos = sourceName => line => column => SourcePos(sourceName, line, column)

    val initialPos: SourceName => SourcePos = sourceName => newPos(sourceName)(1)(1)

    val sourceName: SourcePos => SourceName = pos => pos.name
    val sourceLine: SourcePos => Line = pos => pos.line
    val sourceColumn: SourcePos => Column = pos => pos.column

    val incSourceLine: SourcePos => Line => SourcePos = pos => n => pos.copy(line = pos.line + n)
    val incSourceColumn: SourcePos => Column => SourcePos = pos => n => pos.copy(column = pos.column + n)

    val setSourceName: SourcePos => SourceName => SourcePos = pos => n => pos.copy(name = n)
    val setSourceLine: SourcePos => Line => SourcePos = pos => n => pos.copy(line = n)
    val setSourceColumn: SourcePos => Column => SourcePos = pos => n => pos.copy(column = n)

    val updatePosString: SourcePos => String_ => SourcePos = pos => string => List.foldl(updatePosChar)(pos)(string)

    val updatePosChar: SourcePos => Char => SourcePos = pos => {
        case '\n' => pos.copy(line = pos.line + 1, column = 1)
        case '\t' => pos.copy(column = pos.column + 8 - ((pos.column - 1) % 8))
        case _ => pos.copy(column = pos.column + 1)
    }

    val forcePos: SourcePos => SourcePos = pos => seq(pos.line)(seq(pos.column)(pos)) // no effects

    private[ken] def showSourcePos(name: SourceName, line: Line, column: Column): String_ = {
        def showLineColumn: String_ = "(line " ::: show(line) ::: ", column " ::: show(column) ::: List.from(")")
        if (name == "") {
            showLineColumn
        } else {
            "\"" ::: name ::: "\" " ::: showLineColumn
        }
    }
}
