

// Copyright Shunsuke Sogame 2011.
//
// (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package parsec


private[parsec] trait Prim[s, u, n[+_]] { this: ParsecTOp[s, u, n] =>
    lazy val unexpected: String => ParsecT[s, u, n, Nothing] = msg => ParsecT { new UnParser[s, u, n, Nothing] {
        override def apply[b](v: UnParserParam[s, u, n, Nothing, b]): n[b] = {
            v.eerr { newErrorMessage(UnExpect(msg))(statePos(v.state)) }
        }
    } }

    def unParser[a, b](p: ParsecT[s, u, n, a])(s: State[s, u])
        (cok_ : a => State[s, u] => ParseError => n[b])
        (cerr_ : ParseError => n[b])
        (eok_ : a => State[s, u] => ParseError => n[b])
        (eerr_ : ParseError => n[b]): n[b] =
    {
        p {
            new UnParserParam[s, u, n, a, b] {
                override val state = s
                override val cok = cok_
                override val cerr = cerr_
                override val eok = eok_
                override val eerr = eerr_
            }
        }
    }

    def runParsecT[a](p: ParsecT[s, u, n, a])(s: State[s, u]): n[Consumed_[n[Reply[s, u, a]]]] = {
        import innerMonad.`return`
        def cok(a: a)(s_ : State[s, u])(err: ParseError) = `return`(Consumed(`return`(Ok(a, s_, err).up)).up)
        def cerr(err: ParseError) = `return`(Consumed(`return`(Error(err).up)).up)
        def eok(a: a)(s_ : State[s, u])(err: ParseError) = `return`(Empty(`return`(Ok(a, s_, err).up)).up)
        def eerr(err: ParseError) = `return`(Empty(`return`(Error(err).up)).up)
        unParser(p)(s)(cok)(cerr)(eok)(eerr)
    }

    def mkPT[a](k: State[s, u] => n[Consumed_[n[Reply[s, u, a]]]]): ParsecT[s, u, n, a] = ParsecT { new UnParser[s, u, n, a] {
        override def apply[b](v: UnParserParam[s, u, n, a, b]): n[b] = {
            for {
                cons <- k(v.state)
            } {
                cons match {
                    case Consumed(mrep) => for {
                        rep <- mrep
                    } {
                        rep match {
                            case Ok(x, s_, err) => v.cok(x)(s_)(err)
                            case Error(err) => v.cerr(err)
                        }
                    }
                    case Empty(mrep) => for {
                        rep <- mrep
                    } {
                        rep match {
                            case Ok(x, s_, err) => v.eok(x)(s_)(err)
                            case Error(err) => v.eerr(err)
                        }
                    }
                }
            }
        }
    } }

    // epsilon
    def parserReturn[a](x: a): ParsecT[s, u, n, a] = ParsecT { new UnParser[s, u, n, a] {
        override def apply[b](v: UnParserParam[s, u, n, a, b]): n[b] = v.eok(x)(v.state)(unknownError(v.state))
    } }

    // sequence
    def parserBind[a, z](m: ParsecT[s, u, n, a])(k: a => ParsecT[s, u, n, z]): ParsecT[s, u, n, z] = ParsecT { new UnParser[s, u, n, z] {
        override def apply[b](v: UnParserParam[s, u, n, z, b]): n[b] = {
            def mcok(x: a)(s: State[s, u])(err: ParseError): n[b] = {
                val pcok = v.cok
                val pcerr = v.cerr
                val peok: v.eok = x => s => err_ => v.cok(x)(s)(mergeError(err)(err_))
                val peerr: v.eerr = err_ => v.cerr(mergeError(err)(err_))
                unParser(k(x))(s)(pcok)(pcerr)(peok)(peerr)
            }
            def meok(x: a)(s: State[s, u])(err: ParseError): n[b] = {
                val pcok = v.cok
                val peok: v.eok = x => s => err_ => v.eok(x)(s)(mergeError(err)(err_))
                val pcerr = v.cerr
                val peerr: v.eerr = err_ => v.eerr(mergeError(err)(err_))
                unParser(k(x))(s)(pcok)(pcerr)(peok)(peerr)
            }
            val mcerr = v.cerr
            val meerr = v.eerr
            unParser(m)(v.state)(mcok)(mcerr)(meok)(meerr)
        }
    } }

    // always-fail
    def parserZero: ParsecT[s, u, n, Nothing] = ParsecT { new UnParser[s, u, n, Nothing] {
        override def apply[b](v: UnParserParam[s, u, n, Nothing, b]): n[b] = v.eerr { unknownError(v.state) }
    } }

    // alternative
    def parserPlus[a](m: ParsecT[s, u, n, a])(n: ParsecT[s, u, n, a]): ParsecT[s, u, n, a] = ParsecT { new UnParser[s, u, n, a] {
        override def apply[b](v: UnParserParam[s, u, n, a, b]): n[b] = {
            val meerr: v.eerr = err => {
                val neok: v.eok = y => s_ => err_ => v.eok(y)(s_)(mergeError(err)(err_))
                val neerr: v.eerr = err_ => v.eerr { mergeError(err)(err_) }
                unParser(n)(v.state)(v.cok)(v.cerr)(neok)(neerr)
            }
            unParser(m)(v.state)(v.cok)(v.cerr)(v.eok)(meerr)
        }
    } }

    def label[a](p: ParsecT[s, u, n, a])(msg: String): ParsecT[s, u, n, a] = labels(p)(List(msg))

    def labels[a](p: ParsecT[s, u, n, a])(msgs: List[String]): ParsecT[s, u, n, a] = ParsecT { new UnParser[s, u, n, a] {
        override def apply[b](v: UnParserParam[s, u, n, a, b]): n[b] = {
            def setExpectErrors(err: ParseError)(msgs: List[String]): ParseError = msgs match {
                case Nil => setErrorMessage(Expect(""))(err)
                case msg !:: Nil => setErrorMessage(Expect(msg))(err)
                case msg :: msgs => List.foldr[String, ParseError](msg => err => addErrorMessage(Expect(msg))(err))(setErrorMessage(Expect(msg))(err))(msgs.!)
            }

            val eok_ : v.eok = x => s_ => error => v.eok(x)(s_) {
                if (errorIsUnknown(error)) error
                else setExpectErrors(error)(msgs)
            }
            val eerr_ : v.eerr = err => v.eerr { setExpectErrors(err)(msgs) }
            unParser(p)(v.state)(v.cok)(v.cerr)(eok_)(eerr_)
        }
    } }

    def tokens[t](showTokens: List[t] => String)
        (nextposs: SourcePos => List[t] => SourcePos)
        (tts: List[t])
        (implicit i: Stream[s, n, t], j: Eq[t]): ParsecT[s, u, n, List[t]] = tts match
    {
        case Nil => ParsecT { new UnParser[s, u, n, List[t]] {
            override def apply[b](v: UnParserParam[s, u, n, List[t], b]): n[b] = v.eok(Nil)(v.state) { unknownError(v.state) }
        } }
        case tts @ (tok :: toks) => ParsecT { new UnParser[s, u, n, List[t]] {
            override def apply[b](v: UnParserParam[s, u, n, List[t], b]): n[b] = v.state match {
                case state @ State(input, pos, u) => {
                    val errEof = setErrorMessage(Expect(showTokens(tts)))(newErrorMessage(SysUnExpect(""))(pos))
                    def errExpect(x: t) = setErrorMessage(Expect(showTokens(tts)))(newErrorMessage(SysUnExpect(showTokens(List(x))))(pos))

                    def ok(rs: s): n[b] = {
                        val pos_ = nextposs(pos)(tts)
                        val s_ = State(rs, pos_, u)
                        v.cok(tts)(s_)(newErrorUnknown(pos_))
                    }

                    def walk(ts: List[t])(rs: s): n[b] = (ts, rs) match {
                        case (Nil, rs) => ok(rs)
                        case (t :: ts, rs) => for {
                            sr <- i.uncons(rs)
                        } {
                            sr match {
                                case Nothing => v.cerr { errEof }
                                case Just((x, xs)) => {
                                    if (t === x) walk(ts)(xs)
                                    else v.cerr { errExpect(x) }
                                }
                            }
                        }
                    }

                    for {
                        sr <- i.uncons(input)
                    } {
                        sr match {
                            case Nothing => v.eerr { errEof }
                            case Just((x, xs)) => {
                                if (tok === x) walk(toks)(xs)
                                else v.eerr { errExpect(x) }
                            }
                        }
                    }
                }
            }
        } }
    }

    def `try`[a](p: ParsecT[s, u, n, a]): ParsecT[s, u, n, a] = ParsecT { new UnParser[s, u, n, a] {
        override def apply[b](v: UnParserParam[s, u, n, a, b]): n[b] = v.state match {
            case s @ State(_, pos, _) => {
                val pcerr: v.cerr = parseError => v.eerr { setErrorPos(pos)(parseError) }
                unParser(p)(s)(v.cok)(pcerr)(v.eok)(v.eerr)
            }
        }
    } }

    def token[a, t](showToken: t => String)
        (tokpos: t => SourcePos)
        (test: t => Maybe[a])(implicit i: Stream[s, n, t], ev: Iso1[n, WeakIdentity.apply]): ParsecT[s, u, n, a] =
    {
        def nextpos(* : SourcePos)(tok: t)(ts: s): SourcePos = ev.imply(i.uncons(ts)) match {
            case Nothing => tokpos(tok)
            case Just((tok_, _)) => tokpos(tok_)
        }
        tokenPrim(showToken)(nextpos)(test)
    }

    def tokenPrim[a, t](showToken: t => String)
        (nextpos: SourcePos => t => s => SourcePos)
        (test: t => Maybe[a])
        (implicit i: Stream[s, n, t]): ParsecT[s, u, n, a] =
    {
        tokenPrimEx(showToken)(nextpos)(Nothing)(test)(i)
    }

    def tokenPrimEx[a, t](showToken: t => String)
        (nextpos: SourcePos => t => s => SourcePos)
        (nextstate: Maybe[SourcePos => t => s => u => u])
        (test: t => Maybe[a])
        (implicit i: Stream[s, n, t]): ParsecT[s, u, n, a] =
    {
        nextstate match {
            case Nothing => ParsecT { new UnParser[s, u, n, a] {
                override def apply[b](v: UnParserParam[s, u, n, a, b]): n[b] = v.state match {
                    case State(input, pos, user) => for {
                        r <- i.uncons(input)
                    } {
                        r match {
                            case Nothing => v.eerr { unexpectError("")(pos) }
                            case Just((c, cs)) => test(c) match {
                                case Just(x) => {
                                    val newpos = nextpos(pos)(c)(cs)
                                    val newstate = State(cs, newpos, user)
                                    v.cok(x)(newstate)(newErrorUnknown(newpos))
                                }
                                case Nothing => v.eerr { unexpectError(showToken(c))(pos) }
                            }
                        }
                    }
                }
            } }
            case Just(nextState) => ParsecT { new UnParser[s, u, n, a] {
                override def apply[b](v: UnParserParam[s, u, n, a, b]): n[b] = v.state match {
                    case State(input, pos, user) => for {
                        r <- i.uncons(input)
                    } {
                        r match {
                            case Nothing => v.eerr { unexpectError("")(pos) }
                            case Just((c, cs)) => test(c) match {
                                case Just(x) => {
                                    val newpos = nextpos(pos)(c)(cs)
                                    val newUser = nextState(pos)(c)(cs)(user)
                                    val newstate = State(cs, newpos, newUser)
                                    v.cok(x)(newstate) { newErrorUnknown(newpos) }
                                }
                                case Nothing => v.eerr { unexpectError(showToken(c))(pos) }
                            }
                        }
                    }
                }
            } }
        }
    }

    def many[a](p: ParsecT[s, u, n, a]): ParsecT[s, u, n, List[a]] = {
        import parsecMonad.`for`
        for { xs <- manyAccum(List.op_::[a])(p) } yield List.reverse(xs)
    }

    lazy val skipMany: ParsecT[s, u, n, Any] => ParsecT[s, u, n, Unit] = p => {
        import parsecMonad.`for`
        for { _ <- manyAccum[Any](_ => _ => Nil)(p) } yield ()
    }

    def manyAccum[a](acc: a => Lazy[List[a]] => List[a])(p: ParsecT[s, u, n, a]): ParsecT[s, u, n, List[a]] = ParsecT { new UnParser[s, u, n, List[a]] {
        override def apply[b](v: UnParserParam[s, u, n, List[a], b]): n[b] = {
            def walk(xs: List[a])(x: a)(s_ : State[s, u])(err: ParseError): n[b] = {
                unParser(p)(s_)(walk { acc(x)(xs) })(v.cerr)(manyErr)(e => v.cok(acc(x)(xs))(s_)(e))
            }
            def manyErr(x: a)(s_ : State[s, u])(err: ParseError): n[b] = {
                error("ParsecT.many: combinator 'many' is applied to a parser that accepts an empty string.")
            }
            unParser(p)(v.state)(walk(Nil))(v.cerr)(manyErr)(e => v.eok(Nil)(v.state)(e))
        }
    } }

    // runParserT is equivalent to runParser by the power of Scala.
    def runParser[a](p: ParsecT[s, u, n, a])(u: u)(name: SourceName)(s: s): n[Either[ParseError, a]] = {
        def parserReply(res: Consumed_[n[Reply[s, u, a]]]): n[Reply[s, u, a]] = res match {
            case Consumed(r) => r
            case Empty(r) => r
        }

        import innerMonad.`return`
        for {
            res <- runParsecT(p)(State(s, initialPos(name), u))
            r <- parserReply(res)
        } {
            r match {
                case Ok(x, _, _) => `return`(Right(x))
                case Error(err) => `return`(Left(err))
            }
        }
    }

    def parse[a](p: ParsecT[s, u, n, a])(name: SourceName)(s: s)(implicit ev: Unit =:= u): n[Either[ParseError, a]] = runParser(p)(())(name)(s)

    def parseTest[a](p: ParsecT[s, u, n, a])(input: s)(implicit i: Show[a], ev: Unit =:= u, evi: Iso1[n, WeakIdentity.apply]): IO[Unit] = {
        evi.imply(parse(p)("")(input)) match {
            case Left(err) => for { _ <- IO.putStr("parse error at ") } { IO.print(err) }
            case Right(x) => IO.print(x)
        }
    }

    // Setter/Getter
    //
    import parsecMonad.`for`

    lazy val getPosition: ParsecT[s, u, n, SourcePos] = for { state <- getParserState } yield statePos(state)

    lazy val getInput: ParsecT[s, u, n, s] = for { state <- getParserState } yield stateInput(state)

    lazy val setPosition: SourcePos => ParsecT[s, u, n, Unit] = pos => {
        for { _ <- updateParserState {
            case State(input, _, user) => State(input, pos, user)
        } } yield ()
    }

    lazy val setInput: s => ParsecT[s, u, n, Unit] = input => {
        for { _ <- updateParserState {
            case State(_, pos, user) => State(input, pos, user)
            }
        } yield ()
    }

    lazy val getParserState: ParsecT[s, u, n, State[s, u]] = updateParserState(id)

    lazy val setParserState: State[s, u] => ParsecT[s, u, n, State[s, u]] = st => updateParserState(const(st))

    lazy val updateParserState: (State[s, u] => State[s, u]) => ParsecT[s, u, n, State[s, u]] = f => ParsecT { new UnParser[s, u, n, State[s, u]] {
        override def apply[b](v: UnParserParam[s, u, n, State[s, u], b]): n[b] = {
            val s_ = f(v.state)
            v.eok(s_)(s_) { unknownError(s_) }
        }
    } }

    lazy val getState: ParsecT[s, u, n, u] = parsecMonad.liftM[State[s, u], u](stateUser)(getParserState)

    lazy val putState: u => ParsecT[s, u, n, Unit] = u => {
        for { _ <- updateParserState { (s: State[s, u]) => s.copy(user = u) } } yield ()
    }

    lazy val modifyState: (u => u) => ParsecT[s, u, n, Unit] = f => {
        for { _ <- updateParserState { (s: State[s, u]) => s.copy(user = f(stateUser(s))) } } yield ()
    }

    lazy val setState: u => ParsecT[s, u, n, Unit] = putState
    lazy val updateState: (u => u) => ParsecT[s, u, n, Unit] = modifyState
}
