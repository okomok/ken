

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


object Parsec {

// Pos
    type SourceName = String
    type Line = Int
    type Column = Int

    final case class SourcePos(name: SourceName, line: Line, column: Column)

    def sourceName(pos: SourcePos): SourceName = pos.name
    def sourceLine(pos: SourcePos): Line = pos.line
    def sourceColumn(pos: SourcePos): Column = pos.column

    def incSourceLine(pos: SourcePos)(n: Line): SourcePos = pos.copy(line = pos.line + n)
    def incSourceColumn(pos: SourcePos)(n: Column): SourcePos = pos.copy(column = pos.column + n)

    def setSourceName(pos: SourcePos)(n: SourceName): SourcePos = pos.copy(name = n)
    def setSourceLine(pos: SourcePos)(n: Line): SourcePos = pos.copy(line = n)
    def setSourceColumn(pos: SourcePos)(n: Column): SourcePos = pos.copy(column = n)

// Prim
    type Parser[+a] = GenParser[Char, Unit, a]

    final case class GenParser[tok, st, +a](parse: State[tok, st] => ConsumedT[Reply[tok, st, a]])
    val Parser = GenParser

    def runP[tok, st, a](p: GenParser[tok, st, a]): State[tok, st] => ConsumedT[Reply[tok, st, a]] = p.parse

    sealed abstract class ConsumedT[+a]
    final case class Consumed[+a](_1: a) extends ConsumedT[a]
    final case class Empty[+a](_2: a) extends ConsumedT[a]

    sealed abstract class Reply[+tok, +st, +a]
    final case class Ok[+tok, +st, +a](x: a, state: State[tok, st], err: ParseError) extends Reply[tok, st, a]
    final case class Error(err: ParseError) extends Reply[Nothing, Nothing, Nothing]

    final case class State[+tok, +st](input: List[tok], pos: SourcePos, user: st)

    def stateInput[tok, st](state: State[tok, st]): List[tok] = state.input
    def statePos[tok, st](state: State[tok, st]): SourcePos = state.pos
    def stateUser[tok, st](state: State[tok,st]): st = state.user

    object GenParser {
        implicit def monadInstance[tok, st]: MonadPlus[({type m[x] = GenParser[tok, st, x]})#m] = new MonadPlus[({type m[x] = GenParser[tok, st, x]})#m] {
            private[this] type m[x] = GenParser[tok, st, x]
            // Monad
            override def `return`[a](x: => a): m[a] = parsecReturn(x)
            override def op_>>=[a, b](p: m[a])(f: a => m[b]): m[b] = parsecBind(p)(f)
            // MonadPlus
            override def mzero[a]: m[a] = parsecZero
            override def mplus[a](x: m[a])(y: => m[a]): m[a] = parsecPlus(x)(y)
        }
    }

    def parsecReturn[tok, st, a](x: a): GenParser[tok, st, a] = {
        Parser { (state: State[tok, st]) =>
            Empty(Ok(x, state, unknownError(state)))
        }
    }

    def parsecBind[tok, st, a, b](p: GenParser[tok, st, a])(f: a => GenParser[tok, st, b]): GenParser[tok, st, b] = {
        Parser { (state: State[tok, st]) =>
            runP(p)(state) match {
                case Consumed(reply1) => {
                    Consumed {
                        reply1 match {
                            case Ok(x, state1, err1) => {
                                runP(f(x))(state1) match {
                                    case Empty(reply2) => mergeErrorReply(err1)(reply2)
                                    case Consumed(reply2) => reply2
                                }
                            }
                            case Error(err1) => Error(err1)
                        }
                    }
                }
                case Empty(reply1) => {
                    reply1 match {
                        case Ok(x, state1, err1) => {
                            runP(f(x))(state1) match {
                                case Empty(reply2) => Empty(mergeErrorReply(err1)(reply2))
                                case other => other
                            }
                        }
                        case Error(err1) => Empty(Error(err1))
                    }
                }
            }
        }
    }

    def parsecZero[tok, st, a]: GenParser[tok, st, a] = {
        Parser { (state: State[tok, st]) =>
            Empty(Error(unknownError(state)))
        }
    }

    def parsecPlus[tok, st, a](p1: GenParser[tok, st, a])(p2: => GenParser[tok, st, a]): GenParser[tok, st, a] = {
        Parser { (state: State[tok, st]) =>
            runP(p1)(state) match {
                case Empty(Error(err)) => {
                    runP(p2)(state) match {
                        case Empty(reply) => Empty(mergeErrorReply(err)(reply))
                        case consumed => consumed
                    }
                }
                case other => other
            }
        }
    }

    def mergeErrorReply[tok, st, a](err1: ParseError)(reply: Reply[tok, st, a]) = reply match {
        case Ok(x, state, err2) => Ok(x, state, (mergeError(err1)(err2)))
        case Error(err2) => Error(mergeError(err1)(err2))
    }

    def unknownError[tok, st](state: State[tok, st]): ParseError = newErrorUnknown(statePos(state))

// Error
    sealed abstract class MessageT
    final case class SysUnExpect(s: String) extends MessageT
    final case class UnExpect(s: String) extends MessageT
    final case class Expect(s: String) extends MessageT
    final case class Message(s: String) extends MessageT

    final case class ParseError(pos: SourcePos, msg: List[MessageT])
    def errorPos(err: ParseError): SourcePos = err.pos
    def errorMessage(err: ParseError): List[MessageT] = err.msg
    def errorIsUnknown(err: ParseError): Boolean = List.`null`(err.msg)

    def newErrorUnknown(pos: SourcePos): ParseError = ParseError(pos, Nil)

    def mergeError(err1: ParseError)(err2: ParseError): ParseError = (err1, err2) match {
        case (ParseError(pos, msg1), ParseError(_, msg2)) => ParseError(pos, msg1 ::: msg2)
    }

// Combinators
    import Monad._

    def choice[tok, st, a](ps: List[GenParser[tok, st, a]]): GenParser[tok, st, a] = {
        type m[a] = GenParser[tok, st, a]
        List.foldr[m[a], m[a]](op_<|>)(mzero(monadPlus[m]))(ps)
    }

    def option[tok, st, a](x: a)(p: GenParser[tok, st, a]): GenParser[tok, st, a] = {
        type m[a] = GenParser[tok, st, a]
        (p: m[a]) <|> `return`(x)(monad[m])
    }

    def optional[tok, st, a](p: GenParser[tok, st, a]): GenParser[tok, st, Unit] = {
        type m[a] = GenParser[tok, st, a]
        ( for { _ <- (p: m[a]) } `return`()(monad[m]) ) <|> `return`()(monad[m])
    }
}
