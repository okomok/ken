

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


// Copyright 1999-2000, Daan Leijen. All rights reserved.


package com.github.okomok
package ken


import scala.annotation.tailrec


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

    def updatePosString(pos: SourcePos)(string: String): SourcePos = List.foldl(updatePosChar)(pos)(string)

    def updatePosChar(pos: SourcePos)(c: Char): SourcePos = c match {
        case '\n' => pos.copy(line = pos.line + 1, column =1)
        case '\t' => pos.copy(column = pos.column + 8 - ((pos.column - 1) % 8))
        case _ => pos.copy(column = pos.column + 1)
    }

    def forcePos(pos: SourcePos): SourcePos = seq(pos.line)(seq(pos.column)(pos)) // no effects

// Prim
    type Parser[+a] = GenParser[Char, Unit, a]

    final case class GenParser[tok, st, +a](parse: State[tok, st] => ConsumedT[Reply[tok, st, a]]) extends
        MonadPlusObj[({type m[+x] = GenParser[tok, st, x]})#m, a]
    {
        override val obj = this
    }

    val Parser = GenParser

    def runP[tok, st, a](p: GenParser[tok, st, a])(state: State[tok, st]): ConsumedT[Reply[tok, st, a]] = p.parse(state)

    sealed abstract class ConsumedT[+a] extends Up[ConsumedT[a]]
    final case class Consumed[+a](_1: a) extends ConsumedT[a]
    final case class Empty[+a](_2: a) extends ConsumedT[a]

    sealed abstract class Reply[+tok, +st, +a] extends Up[Reply[tok, st, a]]
    final case class Ok[+tok, +st, +a](x: a, state: State[tok, st], err: ParseError) extends Reply[tok, st, a]
    final case class Error(err: ParseError) extends Reply[Nothing, Nothing, Nothing]

    final case class State[+tok, +st](input: List[tok], pos: SourcePos, user: st)

    def stateInput[tok, st](state: State[tok, st]): List[tok] = state.input
    def statePos[tok, st](state: State[tok, st]): SourcePos = state.pos
    def stateUser[tok, st](state: State[tok,st]): st = state.user

    object GenParser {
        implicit def monad[tok, st]: MonadPlus[({type m[x] = GenParser[tok, st, x]})#m] = new MonadPlus[({type m[x] = GenParser[tok, st, x]})#m] {
            private[this] type m[x] = GenParser[tok, st, x]
            // Monad
            override def `return`[a](x: a): m[a] = parsecReturn(x)
            override def op_>>=[a, b](p: m[a])(f: a => m[b]): m[b] = parsecBind(p)(f)
            // MonadPlus
            override def mzero[a]: m[a] = parsecZero
            override def mplus[a](x: m[a])(y: => m[a]): m[a] = parsecPlus(x)(y)
        }
    }

    /** epsilon **/
    def parsecReturn[tok, st, a](x: a): GenParser[tok, st, a] = {
        Parser { (state: State[tok, st]) =>
            Empty(Ok(x, state, unknownError(state)))
        }
    }

    /** sequential **/
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

    def mergeErrorReply[tok, st, a](err1: ParseError)(reply: Reply[tok, st, a]) = reply match {
        case Ok(x, state, err2) => Ok(x, state, (mergeError(err1)(err2)))
        case Error(err2) => Error(mergeError(err1)(err2))
    }

    /** fail **/
    def parsecZero[tok, st, a]: GenParser[tok, st, a] = {
        Parser { (state: State[tok, st]) =>
            Empty(Error(unknownError(state)))
        }
    }

    /** alternative (non-backtracking) **/
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

    /** make it backtrackable **/
    def `try`[tok, st, a](p: GenParser[tok, st, a]): GenParser[tok, st, a] = {
        Parser { case state@State(input, pos, user) =>
            runP(p)(state) match {
                case Consumed(Error(err)) => Empty(Error(setErrorPos(pos)(err)))
                case Consumed(ok) => Consumed(ok)
                case empty => empty
            }
        }
    }

    def token[tok, st, a](show: tok => String)
        (tokpos: tok => SourcePos)
        (test: tok => Maybe[a]): GenParser[tok, st, a] =
    {
        def nextpos(__ : Any)(tok: tok)(toks: List[tok]): SourcePos = toks match {
            case tok :: _ => tokpos(tok)
            case Nil => tokpos(tok)
        }
        tokenPrim(show)(nextpos)(test)
    }

    def tokenPrim[tok, st, a](show: tok => String)
        (nextpos: SourcePos => tok => List[tok] => SourcePos)
        (test: tok => Maybe[a]): GenParser[tok, st, a] = tokenPrimEx(show)(nextpos)(Nothing)(test)

    def tokenPrimEx[tok, st, a](show: tok => String)
        (nextpos: SourcePos => tok => List[tok] => SourcePos)
        (mbNextState: Maybe[SourcePos => tok => List[tok] => st => st])
        (test: tok => Maybe[a]): GenParser[tok, st, a] =
    {
        mbNextState match {
            case Nothing =>
                Parser { case state@State(input, pos, user) =>
                    input match {
                        case c :: cs => test(c) match {
                            case Just(x) => {
                                val newpos = nextpos(pos)(c)(cs.!)
                                val newstate = State(cs.!, newpos, user)
                                Consumed(Ok(x, newstate, newErrorUnknown(newpos)))
                            }
                            case Nothing => Empty(sysUnExpectError(show(c))(pos))
                        }
                        case Nil => Empty(sysUnExpectError("")(pos))
                    }
                }
            case Just(nextState) =>
                Parser { case state@State(input, pos, user) =>
                    input match {
                        case c :: cs => test(c) match {
                            case Just(x) => {
                                val newpos = nextpos(pos)(c)(cs.!)
                                val newuser = nextState(pos)(c)(cs.!)(user)
                                val newstate = State(cs.!, newpos, newuser)
                                Consumed(Ok(x, newstate, newErrorUnknown(newpos)))
                            }
                            case Nothing => Empty(sysUnExpectError(show(c))(pos))
                        }
                        case Nil => Empty(sysUnExpectError("")(pos))
                    }
                }
        }
    }

    def label[tok, st, a](p: GenParser[tok, st, a])(msg: String): GenParser[tok, st, a] = labels(p)(List(msg))

    def labels[tok, st, a](p: GenParser[tok, st, a])(msgs: List[String]): GenParser[tok, st, a] = {
        Parser { (state: State[tok, st]) =>
            runP(p)(state) match {
                case Empty(reply) => Empty {
                    reply match {
                        case Error(err) => Error(setExpectErrors(err)(msgs))
                        case Ok(x, state1, err) => {
                            if (errorIsUnknown(err)) {
                                reply
                            } else {
                                Ok(x, state1, setExpectErrors(err)(msgs))
                            }
                        }
                    }
                }
                case other => other
            }
        }
    }

    def updateParserState[tok, st](f: State[tok, st] => State[tok, st]): GenParser[tok, st, State[tok, st]] = {
        Parser { (state: State[tok, st]) => {
            val newstate = f(state)
            Empty(Ok(state, newstate, unknownError(newstate)))
        } }
    }

    def unexpected[tok, st](msg: String): GenParser[tok, st, Nothing] = {
        Parser { (state: State[tok, st]) => Empty(Error(newErrorMessage(UnExpect(msg))(statePos(state)))) }
    }

    def setExpectErrors(err: ParseError)(msgs: List[String]): ParseError = msgs match {
        case Nil => setErrorMessage(Expect(""))(err)
        case msg !:: Nil => setErrorMessage(Expect(msg))(err)
        case msg :: msgs => List.foldr[String, ParseError](msg => err => addErrorMessage(Expect(msg))(err))(setErrorMessage(Expect(msg))(err))(msgs.!)
    }

    def sysUnExpectError(msg: String)(pos: SourcePos): Reply[Nothing, Nothing, Nothing] = Error(newErrorMessage(SysUnExpect(msg))(pos))
    def unknownError[tok, st](state: State[tok, st]): ParseError = newErrorUnknown(statePos(state))

    /** star **/
    def many[tok, st, a](p: GenParser[tok, st, a]): GenParser[tok, st, List[a]] = {
        for { xs <- manyAccum(List.op_::[a])(p) } yield List.reverse(xs)
        /*
        type m[a] = GenParser[tok, st, a]
        // For some reason, Monad.`for` doesn't work.
        Monad.op_>>=(manyAccum(List.op_::[a])(p): m[List[a]]) { xs =>
            Monad.`return`(List.reverse(xs))(Monad.monad[m])
        }
        */
    }

    /** star **/
    def skipMany[tok, st](p: GenParser[tok, st, _]): GenParser[tok, st, Unit] = {
        for { xs <- manyAccum[tok, st, Any](x => y => Nil)(p) } yield ()
        /*
        type m[a] = GenParser[tok, st, a]
        // For some reason, Monad.`for` doesn't work.
        Monad.op_>>=(manyAccum[tok, st, Any](x => y => Nil)(p): m[List[_]]) { xs =>
            Monad.`return`(())(Monad.monad[m])
        }
        */
    }

    /** star **/
    def manyAccum[tok, st, a](accum: a => (=> List[a]) => List[a])(p: GenParser[tok, st, a]): GenParser[tok, st, List[a]] = {
        Parser { (state: State[tok, st]) =>
            @tailrec
            def walk(xs: List[a])(state: State[tok, st])(c: ConsumedT[Reply[tok, st, a]]): Reply[tok, st, List[a]] = c match {
                case Empty(Error(err)) => Ok(xs, state, err)
                case Empty(ok) => error("parser shall consume in successful match.")
                case Consumed(Error(err)) => Error(err)
                case Consumed(Ok(x, state_, err)) => {
                    val ys = accum(x)(xs)
                    walk(ys)(state_)(runP(p)(state_))
                }
            }

            runP(p)(state) match {
                case Empty(reply) => reply match {
                    case Ok(x, state_, err) => error("parser shall consume in successful match.")
                    case Error(err) => Empty(Ok(Nil, state, err))
                }
                case consumed => Consumed(walk(Nil)(state)(consumed))
            }
        }
    }

// Error
    sealed abstract class MessageT extends Up[MessageT]
    final case class SysUnExpect(s: String) extends MessageT
    final case class UnExpect(s: String) extends MessageT
    final case class Expect(s: String) extends MessageT
    final case class Message(s: String) extends MessageT

    def messageToEnum(msg: MessageT): Int = msg match {
        case SysUnExpect(_) => 0
        case UnExpect(_) => 1
        case Expect(_) => 2
        case Message(_) => 3
    }

    def messageCompare(msg1: MessageT)(msg2: MessageT): Ordering = {
        Ord.compare(messageToEnum(msg1))(messageToEnum(msg2))
    }

    def messageString(msg: MessageT): String = msg match {
        case SysUnExpect(s) => s
        case UnExpect(s) => s
        case Expect(s) => s
        case Message(s) => s
    }

    def messageEq(msg1: MessageT)(msg2: MessageT): Boolean = {
        messageCompare(msg1)(msg2) == EQ
    }

    final case class ParseError(pos: SourcePos, msgs: List[MessageT])
    def errorPos(err: ParseError): SourcePos = err.pos
    def errorMessage(err: ParseError): List[MessageT] = err.msgs
    def errorIsUnknown(err: ParseError): Boolean = List.`null`(err.msgs)

    def newErrorUnknown(pos: SourcePos): ParseError = ParseError(pos, Nil)
    def newErrorMessage(msg: MessageT)(pos: SourcePos): ParseError = ParseError(pos, List(msg))
    def addErrorMessage(msg: MessageT)(err: ParseError): ParseError = err.copy(msgs = msg :: err.msgs)
    def setErrorPos(pos: SourcePos)(err: ParseError): ParseError = err.copy(pos = pos)
    def setErrorMessage(msg: MessageT)(err: ParseError): ParseError = err.copy(msgs = List.filter(not _ compose messageEq(msg))(err.msgs))
    def mergeError(err1: ParseError)(err2: ParseError): ParseError = ParseError(err1.pos, err1.msgs ::: err2.msgs)

// Combinators
    def choice[tok, st, a](ps: List[GenParser[tok, st, a]]): GenParser[tok, st, a] = {
        import Monad._
        type m[a] = GenParser[tok, st, a]
        List.foldr[m[a], m[a]](op_<|>)(mzero(monadPlus[m]))(ps)
    }

    def option[tok, st, a](x: a)(p: GenParser[tok, st, a]): GenParser[tok, st, a] = {
        p <|> p.`return`(x)
    }

    def optional[tok, st](p: GenParser[tok, st, _]): GenParser[tok, st, Unit] = {
        ( for { _ <- p } yield () ) <|> p.`return`()
    }

    def between[tok, st, a](open: GenParser[tok, st, _])(close: GenParser[tok, st, _])(p: GenParser[tok, st, a]): GenParser[tok, st, a] = {
        for { _ <- open; x <- p; _ <- close } yield x
    }

    def skipMany1[tok, st](p: GenParser[tok, st, _]): GenParser[tok, st, Unit] = {
        for { _ <- p; r <- skipMany1(p) } yield r
    }
    /*
    def skipMany[tok, st](p: GenParser[tok, st, _]): GenParser[tok, st, Unit] = {
        type m[a] = GenParser[tok, st, a]
        lazy val scan: GenParser[tok, st, Unit] = ( for { _ <- p: m[_]; _ <- scan: m[Unit] } yield () ) <|> `return`()(monad[m])
        scan
    }
    */
}
