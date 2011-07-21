

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

    final case class SourcePos(name: SourceName, line: Line, column: Column) {
        override def toString = showSourcePos(name, line, column)
    }

    def newPos(sourceName: SourceName)(line: Line)(column: Column): SourcePos = SourcePos(sourceName, line, column)

    def initialPos(sourceName: SourceName): SourcePos = newPos(sourceName)(1)(1)

    def sourceName(pos: SourcePos): SourceName = pos.name
    def sourceLine(pos: SourcePos): Line = pos.line
    def sourceColumn(pos: SourcePos): Column = pos.column

    def incSourceLine(pos: SourcePos)(n: Line): SourcePos = pos.copy(line = pos.line + n)
    def incSourceColumn(pos: SourcePos)(n: Column): SourcePos = pos.copy(column = pos.column + n)

    def setSourceName(pos: SourcePos)(n: SourceName): SourcePos = pos.copy(name = n)
    def setSourceLine(pos: SourcePos)(n: Line): SourcePos = pos.copy(line = n)
    def setSourceColumn(pos: SourcePos)(n: Column): SourcePos = pos.copy(column = n)

    def updatePosString(pos: SourcePos)(string: String_): SourcePos = List.foldl(updatePosChar)(pos)(string)

    def updatePosChar(pos: SourcePos)(c: Char): SourcePos = c match {
        case '\n' => pos.copy(line = pos.line + 1, column = 1)
        case '\t' => pos.copy(column = pos.column + 8 - ((pos.column - 1) % 8))
        case _ => pos.copy(column = pos.column + 1)
    }

    def forcePos(pos: SourcePos): SourcePos = seq(pos.line)(seq(pos.column)(pos)) // no effects

    private[this] def showSourcePos(name: SourceName, line: Line, column: Column): String = {
        def showLineColumn: String = "(line " + _show(line) + ", column " + _show(column) + ")"
        if (name == "") {
            showLineColumn
        } else {
            "\"" + name + "\" " + showLineColumn
        }
    }

// Prim

    // User state combinators

    def getState[tok, st]: GenParser[tok, st, st] = {
        for { state <- getParserState } yield stateUser(state)
    }

    def setState[tok, st](st: st): GenParser[tok, st, Unit] = {
        for { _ <- updateParserState[tok, st] { case State(input, pos, _) => State(input, pos, st) } } yield ()
    }

    def updateState[tok, st](f: st => st): GenParser[tok, st, Unit] = {
        for { _ <- updateParserState[tok, st] { case State(input, pos, user) => State(input, pos, f(user)) } } yield ()
    }

    // Parser state combinators

    def getPosition[tok, st]: GenParser[tok, st, SourcePos] = {
        for { state <- getParserState } yield statePos(state)
    }

    def getInput[tok, st]: GenParser[tok, st, List[tok]] = {
        for { state <- getParserState } yield stateInput(state)
    }

    def setPosition[tok, st](pos: SourcePos): GenParser[tok, st, Unit] = {
        for { _ <- updateParserState[tok, st] { case State(input, _, user) => State(input, pos, user) } } yield ()
    }

    def setInput[tok, st](input: List[tok]): GenParser[tok, st, Unit] = {
        for { _ <- updateParserState[tok, st] { case State(_, pos, user) => State(input, pos, user) } } yield ()
    }

    def getParserState[tok, st]: GenParser[tok, st, State[tok, st]] = updateParserState(id)
    def setParserState[tok, st](st: State[tok, st]): GenParser[tok, st, State[tok, st]] = updateParserState(const(st))

    // Parser definition

    type Parser[+a] = GenParser[Char, Unit, a]

    sealed abstract class GenParser[tok, st, +a] extends MonadPlusMethod[({type m[+x] = GenParser[tok, st, x]})#m, a] {
        override val klass = GenParser.monad[tok, st]
        override def callee = this

        def parse(st: State[tok, st]): ConsumedT[Reply[tok, st, a]]
        final def <#>(msg: String): GenParser[tok, st, a] = label(this)(msg)
    }

    object Parser {
        def apply[tok, st, a](p: State[tok, st] => ConsumedT[Reply[tok, st, a]]) = new GenParser[tok, st, a] {
            override def parse(st: State[tok, st]): ConsumedT[Reply[tok, st, a]] = p(st)
        }
        def unapply[tok, st, a](p: GenParser[tok, st, a]): Option[State[tok, st] => ConsumedT[Reply[tok, st, a]]] = Some((st: State[tok, st]) => p.parse(st))

        implicit val monad: MonadPlus[({type m[+x] = GenParser[Char, Unit, x]})#m] = GenParser.monad[Char, Unit]
    }

    type Rule[a] = GenRule[Char, Unit, a]

    /** For recursive grammer **/
    final class GenRule[tok, st, a] extends GenParser[tok, st, a] {
        @volatile private[this] var p: Lazy[GenParser[tok, st, a]] = null
        def ::=(that: => GenParser[tok, st, a]): Unit = { p = Lazy(that) }
        override def parse(st: State[tok, st]): ConsumedT[Reply[tok, st, a]] = p.!.parse(st)
    }

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
        implicit def monad[tok, st]: MonadPlus[({type m[+x] = GenParser[tok, st, x]})#m] = new MonadPlus[({type m[+x] = GenParser[tok, st, x]})#m] {
            private[this] type m[+x] = GenParser[tok, st, x]
            // Functor
            override def fmap[a, b](x: a => b)(y: m[a]): m[b] = parsecMap(x)(y)
            // Monad
            override def `return`[a](x: a): m[a] = parsecReturn(x)
            override def op_>>=[a, b](p: m[a])(f: a => m[b]): m[b] = parsecBind(p)(f)
            // MonadPlus
            override def mzero: m[Nothing] = parsecZero
            override def mplus[a](x: m[a])(y: => m[a]): m[a] = parsecPlus(x)(y)
        }
    }

    // Run a parser

    def parseFromFile[a](p: Parser[a], fname: SourceName): IO[Either[ParseError, a]] = {
        for { input <- IO.readFile(fname) } yield parse(p)(fname)(input)
    }

    def parseTest[tok, a](p: GenParser[tok, Unit, a])(input: List[tok]): IO[Unit] = {
        runParser(p)(())("")(input) match {
            case Left(err) => for {
                _ <- IO.putStr("parse error at ")
                _ <- IO.print(err)
            } yield ()
            case Right(x) => IO.print(x)
        }
    }

    def parse[tok, a](p: GenParser[tok, Unit, a])(name: SourceName)(input: List[tok]): Either[ParseError, a] = {
        runParser(p)(())(name)(input)
    }

    def runParser[tok, st, a](p: GenParser[tok, st, a])(st: st)(name: SourceName)(input: List[tok]): Either[ParseError, a] = {
        parserReply(runP(p)(State(input, initialPos(name), st))) match {
            case Ok(x, _, _) => Right(x)
            case Error(err) => Left(err)
        }
    }

    def parserReply[tok, st, a](result: ConsumedT[Reply[tok, st, a]]): Reply[tok, st, a] = {
        result match {
            case Consumed(reply) => reply
            case Empty(reply) => reply
        }
    }

    // Functor

    def parsecMap[tok, st, a, b](f: a => b)(p: GenParser[tok, st, a]): GenParser[tok, st, b] = {
        def mapReply(reply: Reply[tok, st, a]): Reply[tok, st, b] = {
            reply match {
                case Ok(x, state, err) => Ok(f(x), state, err)
                case Error(err) => Error(err)
            }
        }

        Parser { (state: State[tok, st]) =>
            runP(p)(state) match {
                case Consumed(reply) => Consumed(mapReply(reply))
                case Empty(reply) => Empty(mapReply(reply))
            }
        }
    }

    // Monad

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

    // MonadPlus

    /** always fail **/
    def parsecZero[tok, st, Nothing]: GenParser[tok, st, Nothing] = {
        Parser { (state: State[tok, st]) =>
            Empty(Error(unknownError(state)))
        }
    }

    /** p1|p2 (non-backtracking) **/
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

    // Primitive Parsers

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
        def nextpos(* : Any)(tok: tok)(toks: List[tok]): SourcePos = toks match {
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

    /** epsilon with state updating **/
    def updateParserState[tok, st](f: State[tok, st] => State[tok, st]): GenParser[tok, st, State[tok, st]] = {
        Parser { (state: State[tok, st]) => {
            val newstate = f(state)
            Empty(Ok(state, newstate, unknownError(newstate)))
        } }
    }

    /** always fail **/
    def unexpected[tok, st](msg: String): GenParser[tok, st, Nothing] = {
        Parser { (state: State[tok, st]) =>
            Empty(Error(newErrorMessage(UnExpect(msg))(statePos(state))))
        }
    }

    def setExpectErrors(err: ParseError)(msgs: List[String]): ParseError = msgs match {
        case Nil => setErrorMessage(Expect(""))(err)
        case msg !:: Nil => setErrorMessage(Expect(msg))(err)
        case msg :: msgs => List.foldr[String, ParseError](msg => err => addErrorMessage(Expect(msg))(err))(setErrorMessage(Expect(msg))(err))(msgs.!)
    }

    def sysUnExpectError(msg: String)(pos: SourcePos): Reply[Nothing, Nothing, Nothing] = Error(newErrorMessage(SysUnExpect(msg))(pos))
    def unknownError[tok, st](state: State[tok, st]): ParseError = newErrorUnknown(statePos(state))

    // Parsers unfolded for space

    /** star **/
    def many[tok, st, a](p: GenParser[tok, st, a]): GenParser[tok, st, List[a]] = {
        for { xs <- manyAccum(List.op_::[a])(p) } yield List.reverse(xs)
    }

    /** star **/
    def skipMany[tok, st](p: GenParser[tok, st, _]): GenParser[tok, st, Unit] = {
        for { xs <- manyAccum[tok, st, Any](x => y => Nil)(p) } yield ()
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

    // Parsers unfolded for speed

    def tokens[tok, st](shows: List[tok] => String)(nextposs: SourcePos => List[tok] => SourcePos)(s: List[tok]): GenParser[tok, st, List[tok]] = {
        Parser { case state@State(input, pos, user) =>
            def ok(cs: List[tok]): Reply[tok, st, List[tok]] = {
                val newpos = nextposs(pos)(s)
                val newstate = State(cs, newpos, user)
                Ok(s, newstate, newErrorUnknown(newpos))
            }

            def errEof: Reply[tok, st, List[tok]] = Error(setErrorMessage(Expect(shows(s)))(newErrorMessage(SysUnExpect(""))(pos)))
            def errExpect(c: tok): Reply[tok, st, List[tok]] = Error(setErrorMessage(Expect(shows(s)))(newErrorMessage(SysUnExpect(_show(List(c))))(pos)))

            @tailrec
            def walk(xs: List[tok])(cs: List[tok]): Reply[tok, st, List[tok]] = (xs, cs) match {
                case (Nil, cs) => ok(cs)
                case (xs, Nil) => errEof
                case (x :: xs, c :: cs) => if (x == c) walk(xs.!)(cs.!) else errExpect(c)
            }

            def walk1(xs: List[tok])(cs: List[tok]): ConsumedT[Reply[tok, st, List[tok]]] = (xs, cs) match {
                case (Nil, cs) => Empty(ok(cs))
                case (xs, Nil) => Empty(errEof)
                case (x :: xs, c :: cs) => if (x == c) Consumed(walk(xs.!)(cs.!)) else Empty(errExpect(c))
            }

            walk1(s)(input)
        }
    }


// Error

    // Message

    sealed abstract class Message_ extends Up[Message_]
    final case class SysUnExpect(s: String) extends Message_
    final case class UnExpect(s: String) extends Message_
    final case class Expect(s: String) extends Message_
    final case class Message(s: String) extends Message_

    def messageToEnum(msg: Message_): Int = msg match {
        case SysUnExpect(_) => 0
        case UnExpect(_) => 1
        case Expect(_) => 2
        case Message(_) => 3
    }

    def messageCompare(msg1: Message_)(msg2: Message_): Ordering = {
        Ord[Int].compare(messageToEnum(msg1))(messageToEnum(msg2))
    }

    def messageString(msg: Message_): String = msg match {
        case SysUnExpect(s) => s
        case UnExpect(s) => s
        case Expect(s) => s
        case Message(s) => s
    }

    def messageStringT(msg: Message_): String_ = List.from(messageString(msg))

    def messageEq(msg1: Message_)(msg2: Message_): Bool = {
        messageCompare(msg1)(msg2) == EQ
    }

    // ParseErrors

    final case class ParseError(pos: SourcePos, msgs: List[Message_]) {
        override def toString = showParseError(this)
    }
    def errorPos(err: ParseError): SourcePos = err.pos
    def errorMessages(err: ParseError): List[Message_] = List.sortBy(messageCompare)(err.msgs)
    def errorIsUnknown(err: ParseError): Bool = List.`null`(err.msgs)

    // Create ParseErrors

    def newErrorUnknown(pos: SourcePos): ParseError = ParseError(pos, Nil)
    def newErrorMessage(msg: Message_)(pos: SourcePos): ParseError = ParseError(pos, List(msg))
    def addErrorMessage(msg: Message_)(err: ParseError): ParseError = err.copy(msgs = msg :: err.msgs)
    def setErrorPos(pos: SourcePos)(err: ParseError): ParseError = err.copy(pos = pos)
    def setErrorMessage(msg: Message_)(err: ParseError): ParseError = err.copy(msgs = msg :: List.filter(not compose messageEq(msg))(err.msgs))
    def mergeError(err1: ParseError)(err2: ParseError): ParseError = ParseError(err1.pos, err1.msgs ::: err2.msgs)

    // Show ParseErrors

    private[this] def showParseError(err: ParseError): String = {
        _show(errorPos(err)) + ":" +
            showErrorMessages("or")("unknown parse error")("expecting")("unexpected")("end of input")(errorMessages(err))
    }

    def showErrorMessages(msgOr: String_)
        (msgUnknown: String_)(msgExpecting: String_)(msgUnExpected: String_)
        (msgEndOfInput: String_)(msgs: List[Message_]): String =
    {
        val (sysUnExpect, msgs1) = List.span(messageEq(SysUnExpect("")))(msgs)
        val (unExpect, msgs2) = List.span(messageEq(UnExpect("")))(msgs1)
        val (expect, messages) = List.span(messageEq(Expect("")))(msgs2)

        def clean(ms: List[String_]): List[String_] = List.nub(List.filter[String_](not compose List.`null`)(ms))

        def separate(sep: String_)(ms: List[String_]): String_ = ms match {
            case Nil => Nil
            case m !:: Nil => m
            case m :: ms => m ::: sep ::: separate(sep)(ms.!)
        }

        def commasOr(s: List[String_]): String_ = s match {
            case Nil => Nil
            case m !:: Nil => m
            case ms => commaSep(List.init(ms)) ::: " " ::: msgOr ::: " " ::: List.last(ms)
        }

        def commaSep(ms: List[String_]): String_ = separate(", ")(clean(ms))
        def semiSep(ms: List[String_]): String_ = separate("; ")(clean(ms))

        def showMany(pre: String_)(msgs: List[Message_]): String_ = {
            clean(List.map(messageStringT)(msgs)) match {
                case Nil => ""
                case ms => {
                    if (List.`null`(pre)) {
                        commasOr(ms)
                    } else {
                        pre ::: " " ::: commasOr(ms)
                    }
                }
            }
        }

        def showExpect: String_ = showMany(msgExpecting)(expect)
        def showUnExpect: String_ = showMany(msgUnExpected)(unExpect)
        def showSysUnExpect: String_ = {
            def firstMsg: String_ = messageStringT(List.head(sysUnExpect))

            if (not(List.`null`(unExpect)) || List.`null`(sysUnExpect)) {
                ""
            } else if (List.`null`(firstMsg)) {
                msgUnExpected ::: " " ::: msgEndOfInput
            } else {
                msgUnExpected ::: " " ::: firstMsg
            }
        }
        def showMessages: String_ = showMany("")(messages)

        val r = if (List.`null`(msgs)) {
            msgUnknown
        } else {
            List.concat { List.map((m: String_) => "\n" ::: m) { clean {
                List(showSysUnExpect, showUnExpect, showExpect, showMessages)
            } } }
        }

        List.stringize(r)
    }


// Combinators

    /** p1|p2|...|pn **/
    def choice[tok, st, a](ps: List[GenParser[tok, st, a]]): GenParser[tok, st, a] = {
        val i = GenParser.monad[tok, st]
        import i._
        List.foldr[apply[a], apply[a]](op_<|>)(mzero)(ps)
    }

    /** p? **/
    def option[tok, st, a](x: a)(p: GenParser[tok, st, a]): GenParser[tok, st, a] = {
        val i = GenParser.monad[tok, st]
        p <|> i.`return`(x)
    }

    def optional[tok, st](p: GenParser[tok, st, _]): GenParser[tok, st, Unit] = {
        val i = GenParser.monad[tok, st]
        ( for { _ <- p } yield () ) <|> i.`return`()
    }

    /** open p close **/
    def between[tok, st, a](open: GenParser[tok, st, _])(close: GenParser[tok, st, _])(p: GenParser[tok, st, a]): GenParser[tok, st, a] = {
        for { _ <- open; x <- p; _ <- close } yield x
    }

    /** p+ (result abandoned) **/
    def skipMany1[tok, st](p: GenParser[tok, st, _]): GenParser[tok, st, Unit] = {
        for { _ <- p; * <- skipMany(p) } yield *
    }
    /*
    def skipMany[tok, st](p: GenParser[tok, st, _]): GenParser[tok, st, Unit] = {
        type m[a] = GenParser[tok, st, a]
        lazy val scan: GenParser[tok, st, Unit] = ( for { _ <- p: m[_]; _ <- scan: m[Unit] } yield () ) <|> `return`()(monad[m])
        scan
    }
    */

    /** p+ **/
    def many1[tok, st, a](p: GenParser[tok, st, a]): GenParser[tok, st, List[a]] = {
        for { x <- p; xs <- many(p) } yield (x :: xs)
    }

    /** p (sep p)* **/
    def sepBy1[tok, st, a](p: GenParser[tok, st, a])(sep: GenParser[tok, st, _]): GenParser[tok, st, List[a]] = {
        for { x <- p; xs <- many(sep >> p) } yield (x :: xs)
    }

    def sepBy[tok, st, a](p: GenParser[tok, st, a])(sep: GenParser[tok, st, _]): GenParser[tok, st, List[a]] = {
        val i = GenParser.monad[tok, st]
        sepBy1(p)(sep) <|> i.`return`(Nil)
    }

    def sepEndBy1[tok, st, a](p: GenParser[tok, st, a])(sep: GenParser[tok, st, _]): GenParser[tok, st, List[a]] = {
        val i = GenParser.monad[tok, st]
        for {
            x <- p
            y <- ( for { _ <- sep; xs <- sepEndBy(p)(sep) } yield (x :: xs) ) <|> i.`return`(List(x))
        } yield y
    }

    def sepEndBy[tok, st, a](p: GenParser[tok, st, a])(sep: GenParser[tok, st, _]): GenParser[tok, st, List[a]] = {
        val i = GenParser.monad[tok, st]
        sepEndBy1(p)(sep) <|> i.`return`(Nil)
    }

    /** (p sep)+ **/
    def endBy1[tok, st, a](p: GenParser[tok, st, a])(sep: GenParser[tok, st, _]): GenParser[tok, st, List[a]] = {
        many1 { for { x <- p; _ <- sep } yield x }
    }

    /** (p sep)* **/
    def endBy[tok, st, a](p: GenParser[tok, st, a])(sep: GenParser[tok, st, _]): GenParser[tok, st, List[a]] = {
        many { for { x <- p; _ <- sep } yield x }
    }

    /** p{n} **/
    def count[tok, st, a](n: Int)(p: GenParser[tok, st, a]): GenParser[tok, st, List[a]] = {
        val i = GenParser.monad[tok, st]
        if (n <= 0) i.`return`(Nil) else i.sequence(List.replicate(n)(p))
    }

    /** folding with seed **/
    def chainr[tok, st, a](p: GenParser[tok, st, a])(op: GenParser[tok, st, a => a => a])(x: a): GenParser[tok, st, a] = {
        val i = GenParser.monad[tok, st]
        chainr1(p)(op) <|> i.`return`(x)
    }

    /** folding with seed **/
    def chainl[tok, st, a](p: GenParser[tok, st, a])(op: GenParser[tok, st, a => a => a])(x: a): GenParser[tok, st, a] = {
        val i = GenParser.monad[tok, st]
        chainl1(p)(op) <|> i.`return`(x)
    }

    /** folding without seed **/
    def chainr1[tok, st, a](p: GenParser[tok, st, a])(op: GenParser[tok, st, a => a => a]): GenParser[tok, st, a] = {
        val i = GenParser.monad[tok, st]
        def rest(x: a): GenParser[tok, st, a] = ( for { f <- op; y <- scan } yield f(x)(y) ) <|> i.`return`(x)
        lazy val scan: GenParser[tok, st, a] = for { x <- p; y <- rest(x) } yield y
        scan
    }

    /** folding without seed **/
    def chainl1[tok, st, a](p: GenParser[tok, st, a])(op: GenParser[tok, st, a => a => a]): GenParser[tok, st, a] = {
        val i = GenParser.monad[tok, st]
        def rest(x: a): GenParser[tok, st, a] = ( for { f <- op; y <- p; z <- rest(f(x)(y)) } yield z ) <|> i.`return`(x)
        for { x <- p; y <- rest(x) } yield y
    }

    // Tricky combinators

    def anyToken[tok, st]: GenParser[tok, st, tok] = {
        tokenPrim[tok, st, tok](_show)(pos => tok => toks => pos)(Maybe.just)
    }

    def eof[tok, st]: GenParser[tok, st, Unit] = {
        notFollowedBy[tok, st](anyToken) <#> "end of input"
    }

    /** negative lookahead **/
    def notFollowedBy[tok, st](p: GenParser[tok, st, tok]): GenParser[tok, st, Unit] = {
        val i = GenParser.monad[tok, st]
        `try` {
            ( for { c <- p; y <- unexpected(_show(List(c))) } yield y ) <|> i.`return`()
        }
    }

    /** star-until **/
    def manyTill[tok, st, a](p: GenParser[tok, st, a])(end: GenParser[tok, st, _]): GenParser[tok, st, List[a]] = {
        lazy val scan: GenParser[tok, st, List[a]] = {
            ( for { _ <- end } yield Nil ) <|> ( for { x <- p; xs <- scan } yield (x :: xs) )
        }
        scan
    }

    /** positive lookahead **/
    def lookAhead[tok, st, a](p: GenParser[tok, st, a]): GenParser[tok, st, a] = {
        for { state <- getParserState; x <- p; _ <- setParserState(state) } yield x
    }


// Char

    // Type of character parsers

    type CharParser[st, +a] = GenParser[Char, st, a]

    // Character parsers

    def oneOf[st](cs: String_): CharParser[st, Char] = satisfy(c => List.elem(c)(cs))
    def noneOf[st](cs: String_): CharParser[st, Char] = satisfy(c => not(List.elem(c)(cs)))

    def spaces[st]: CharParser[st, Unit] = skipMany(space[st]) <#> "white space"

    def space[st]: CharParser[st, Char] = satisfy(_.isSpaceChar) <#> "space"
    def newline[st]: CharParser[st, Char] = char('\n') <#> "new-line"
    def tab[st]: CharParser[st, Char] = char('\t') <#> "tab"

    def upper[st]: CharParser[st, Char] = satisfy(_.isUpper) <#> "uppercase letter"
    def lower[st]: CharParser[st, Char] = satisfy(_.isLower) <#> "lowercase letter"
    def alphaNum[st]: CharParser[st, Char] = satisfy(_.isLetterOrDigit) <#> "letter or digit"
    def letter[st]: CharParser[st, Char] = satisfy(_.isLetter) <#> "letter"
    def digit[st]: CharParser[st, Char] = satisfy(_.isDigit) <#> "digit"
    //def hexDigit[st]: CharParser[st, Char] = satisfy(_.isHexDigit) <#> "hexadecimal digit"
    //def octDigit[st]: CharParser[st, Char] = satisfy(_.isOctDigit) <#> "octal digit"

    def char[st](c: Char): CharParser[st, Char] = satisfy(_ == c) <#> _show(List(c))

    def anyChar[st]: CharParser[st, Char] = satisfy(const(true))

    // Primitive character parsers

    def satisfy[st](f: Char => Bool): CharParser[st, Char] = {
        tokenPrim[Char, st, Char](c => _show(List(c)))(pos => c => cs => updatePosChar(pos)(c))(c => if (f(c)) Just(c) else Nothing)
    }

    def string[st](s: String_): CharParser[st, String_] = tokens[Char, st](_show)(updatePosString)(s)
}
