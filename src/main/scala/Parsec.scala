

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

    trait GenParser[tok, st, +a] {
        def parse: State[tok, st] => Consumed[Reply[tok, st, a]]
    }

    trait GenParserProxy[tok, st, +a] extends GenParser[tok, st, a] with Proxy {
        def self: GenParser[tok, st, a]
        override val parse: State[tok, st] => Consumed[Reply[tok, st, a]] = self.parse
    }

    object Parser {
        def apply[tok, st, a](p: State[tok, st] => Consumed[Reply[tok, st, a]]): GenParser[tok, st, a] = new GenParser[tok, st, a] {
            override val parse = p
        }
        def unapply[tok, st, a](p: GenParser[tok, st, a]): Option[State[tok, st] => Consumed[Reply[tok, st, a]]] = Some(p.parse)
    }

    def runP[tok, st, a](p: GenParser[tok, st, a]): State[tok, st] => Consumed[Reply[tok, st, a]] = p.parse

    sealed abstract class Consumed[+a]
    final case class Consumed_[+a](_1: a) extends Consumed[a]
    final case class Empty[+a](_2: a) extends Consumed[a]

    sealed abstract class Reply[+tok, +st, +a]
    final case class Ok[+tok, +st, +a](x: a, state: State[tok, st], err: ParseError) extends Reply[tok, st, a]
    final case class Error(err: ParseError) extends Reply[Nothing, Nothing, Nothing]

    final case class State[+tok, +st](input: List[tok], pos: SourcePos, user: st)

    def stateInput[tok, st](state: State[tok, st]): List[tok] = state.input
    def statePos[tok, st](state: State[tok, st]): SourcePos = state.pos
    def stateUser[tok, st](state: State[tok,st]): st = state.user

    trait GenParserMonad[tok, st] {
        final class ParserMonadWrap[a](override val self: GenParser[tok, st, a]) extends GenParserProxy[tok, st, a]

        object ParserMonadWrap {
            class MonadInstance extends Monad[ParserMonadWrap] {
                private[this] type m[a] = ParserMonadWrap[a]
                // Monad
                override def `return`[a](x: => a): m[a] = from(parsecReturn(x))
                override def op_>>=[a, b](p: m[a])(f: a => m[b]): m[b] = from(parsecBind(p.self)(x => f(x).self))
            }
            implicit def monadInstance: Monad[ParserMonadWrap] = new MonadInstance
        }

        def from[a](from: GenParser[tok, st, a]): ParserMonadWrap[a] = new ParserMonadWrap(from)
    }

    def parsecReturn[tok, st, a](x: a): GenParser[tok, st, a] = {
        Parser { (state: State[tok, st]) =>
            Empty(Ok(x, state, unknownError(state)))
        }
    }

    def parsecBind[tok, st, a, b](p: GenParser[tok, st, a])(f: a => GenParser[tok, st, b]): GenParser[tok, st, b] = {
        Parser { (state: State[tok, st]) =>
            runP(p)(state) match {
                case Consumed_(reply1) => {
                    Consumed_ {
                        reply1 match {
                            case Ok(x, state1, err1) => {
                                runP(f(x))(state1) match {
                                    case Empty(reply2) => mergeErrorReply(err1)(reply2)
                                    case Consumed_(reply2) => reply2
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

    def mergeErrorReply[tok, st, a](err1: ParseError)(reply: Reply[tok, st, a]) = reply match {
        case Ok(x, state, err2) => Ok(x, state, (mergeError(err1)(err2)))
        case Error(err2) => Error(mergeError(err1)(err2))
    }

    def unknownError[tok, st](state: State[tok, st]): ParseError = newErrorUnknown(statePos(state))

    def usingMonad[tok, st, a, b](p: GenParser[tok, st, a])(f: a => b): GenParser[tok, st, b] = {
        //type g[a] = GenParser_[tok, st]#T[a]
        //val q: g[a] = p
        import Monad.`for`
        new GenParserMonad[tok, st] {
            val r = for {
                x <- from(p)
            } yield f(x)
        }.r
    }

// Error
    sealed abstract class Message
    final case class SysUnExpect(s: String) extends Message
    final case class UnExpect(s: String) extends Message
    final case class Expect(s: String) extends Message
    final case class Message_(s: String) extends Message

    final case class ParseError(pos: SourcePos, msg: List[Message])
    def errorPos(err: ParseError): SourcePos = err.pos
    def errorMessage(err: ParseError): List[Message] = err.msg
    def errorIsUnknown(err: ParseError): Boolean = List.`null`(err.msg)

    def newErrorUnknown(pos: SourcePos): ParseError = ParseError(pos, Nil)

    def mergeError(err1: ParseError)(err2: ParseError): ParseError = (err1, err2) match {
        case (ParseError(pos, msg1), ParseError(_, msg2)) => ParseError(pos, msg1 ::: msg2)
    }
}
