

// Copyright Shunsuke Sogame 2011.
//
// (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


package object parsec {

    // Error
    //
    val messageString: Message_ => String_ = {
        case SysUnExpect(s) => s
        case UnExpect(s) => s
        case Expect(s) => s
        case Message(s) => s
    }

    val errorPos: ParseError => SourcePos = err => err.pos
    val errorMessages: ParseError => List[Message_] = err => List.sort(err.msgs)
    val errorIsUnknown: ParseError => Bool = err => List.`null`(err.msgs)

    val newErrorUnknown: SourcePos => ParseError = pos => ParseError(pos, Nil)
    val newErrorMessage: Message_ => SourcePos => ParseError = msg => pos => ParseError(pos, List(msg))
    val addErrorMessage: Message_ => ParseError => ParseError = msg => err => err.copy(msgs = msg :: err.msgs)
    val setErrorPos: SourcePos => ParseError => ParseError = pos => err => err.copy(pos = pos)
    val setErrorMessage: Message_ => ParseError => ParseError = msg => err => err.copy(msgs = msg :: List.filter(msg /== (_: Message_))(err.msgs))
    val mergeError: ParseError => ParseError => ParseError = err1 => err2 => ParseError(err1.pos, err1.msgs ::: err2.msgs)

    // Pos
    //
    type SourceName = String
    type Line = Int
    type Column = Int

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

    // Prim
    //
    def unknownError[s, u](state: State[s, u]): ParseError = newErrorUnknown(statePos(state))

    def sysUnExpectError(msg: String)(pos: SourcePos): Reply[Nothing, Nothing, Nothing] = Error(newErrorMessage(SysUnExpect(msg))(pos))

    def mergeErrorReply[s, u, a](err1: ParseError)(reply: Reply[s, u, a]) = reply match {
        case Ok(x, state, err2) => Ok(x, state, (mergeError(err1)(err2)))
        case Error(err2) => Error(mergeError(err1)(err2))
    }

    def stateInput[s, u](state: State[s, u]): s = state.input
    def statePos[s, u](state: State[s, u]): SourcePos = state.pos
    def stateUser[s, u](state: State[s,u]): u = state.user

    val unexpectError: String_ => SourcePos => ParseError = msg => pos => newErrorMessage(SysUnExpect(msg))(pos)
}
