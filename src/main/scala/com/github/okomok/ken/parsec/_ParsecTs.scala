

// Copyright Shunsuke Sogame 2011.
//
// (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package parsec


private[ken] final class _ParsecTs[n[+_]](val inner: Monad[n]) {

    // ParsecT
    //
    final case class _ParsecT[s, u, +a](override val get: UnParser[s, u, n, a]) extends NewtypeOf[UnParser[s, u, n, a]] {
        def apply[b](v: UnParserParam[s, u, n, a, b]): n[b] = get(v)

        final def <#>(msg: String_): _ParsecT[s, u, a] = _ParsecT.label(this)(msg)
    }

    object _ParsecT extends _ParsecT_ with Kind.FunctionLike {
        sealed trait apply[s, u] extends Kind.AbstractMonadTrans {
            override type apply1[+a] = _ParsecT[s, u, a]
            override type oldtype1[+a] = UnParser[s, u, n, a]
            override type innerMonad[+a] = n[a]
        }

        implicit def dependent[s, u, a](n: NewtypeOf[UnParser[s, u, n, a]]): _ParsecT[s, u, a] = _ParsecT { n.get }

        // Primitives
        //
        def unParser[s, u, a, b](p: _ParsecT[s, u, a])(s: State[s, u])
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

        def runParsecT[s, u, a](p: _ParsecT[s, u, a])(s: State[s, u]): n[Consumed_[n[Reply[s, u, a]]]] = {
            import inner.`return`
            def cok(a: a)(s_ : State[s, u])(err: ParseError) = `return`(Consumed(`return`(Ok(a, s_, err).up)).up)
            def cerr(err: ParseError) = `return`(Consumed(`return`(Error(err).up)).up)
            def eok(a: a)(s_ : State[s, u])(err: ParseError) = `return`(Empty(`return`(Ok(a, s_, err).up)).up)
            def eerr(err: ParseError) = `return`(Empty(`return`(Error(err).up)).up)
            unParser(p)(s)(cok)(cerr)(eok)(eerr)
        }

        def mkPT[s, u, a](k: State[s, u] => n[Consumed_[n[Reply[s, u, a]]]]): _ParsecT[s, u, a] = _ParsecT { new UnParser[s, u, n, a] {
            override def apply[b](v: UnParserParam[s, u, n, a, b]): n[b] = {
                import inner.`for`
                for {
                    cons <- k(v.state)
                    * <- cons match {
                        case Consumed(mrep) => for {
                            rep <- mrep
                            * <- rep match {
                                case Ok(x, s_, err) => v.cok(x)(s_)(err)
                                case Error(err) => v.cerr(err)
                            }
                        } yield *
                        case Empty(mrep) => for {
                            rep <- mrep
                            * <- rep match {
                                case Ok(x, s_, err) => v.eok(x)(s_)(err)
                                case Error(err) => v.eerr(err)
                            }
                        } yield *
                    }
                } yield *
            }
        } }

        // Epsilon
        //
        def parserReturn[s, u, a, c](x: a): _ParsecT[s, u, a] = _ParsecT { new UnParser[s, u, n, a] {
            override def apply[b](v: UnParserParam[s, u, n, a, b]): n[b] = v.eok(x)(v.state)(unknownError(v.state))
        } }

        // Sequence
        //
        def parserBind[s, u, a, z](m: _ParsecT[s, u, a])(k: a => _ParsecT[s, u, z]): _ParsecT[s, u, z] = _ParsecT { new UnParser[s, u, n, z] {
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

        // Always-fail
        //
        def parserZero[s, u]: _ParsecT[s, u, Nothing] = _ParsecT { new UnParser[s, u, n, Nothing] {
            override def apply[b](v: UnParserParam[s, u, n, Nothing, b]): n[b] = v.eerr { unknownError(v.state) }
        } }

        // Alternative
        //
        def parserPlus[s, u, a](m: _ParsecT[s, u, a])(n: _ParsecT[s, u, a]): _ParsecT[s, u, a] = _ParsecT { new UnParser[s, u, n, a] {
            override def apply[b](v: UnParserParam[s, u, n, a, b]): n[b] = {
                val meerr: v.eerr = err => {
                    val neok: v.eok = y => s_ => err_ => v.eok(y)(s_)(mergeError(err)(err_))
                    val neerr: v.eerr = err_ => v.eerr { mergeError(err)(err_) }
                    unParser(n)(v.state)(v.cok)(v.cerr)(neok)(neerr)
                }
                unParser(m)(v.state)(v.cok)(v.cerr)(v.eok)(meerr)
            }
        } }

        // Label
        //
        def label[s, u, a](p: _ParsecT[s, u, a])(msg: String_): _ParsecT[s, u, a] = labels(p)(List(msg))

        def labels[s, u, a](p: _ParsecT[s, u, a])(msgs: List[String_]): _ParsecT[s, u, a] = _ParsecT { new UnParser[s, u, n, a] {
            override def apply[b](v: UnParserParam[s, u, n, a, b]): n[b] = {
                def setExpectErrors(err: ParseError)(msgs: List[String_]): ParseError = msgs match {
                    case Nil => setErrorMessage(Expect(""))(err)
                    case msg !:: Nil => setErrorMessage(Expect(msg))(err)
                    case msg :: msgs => List.foldr[String_, ParseError](msg => err => addErrorMessage(Expect(msg))(err))(setErrorMessage(Expect(msg))(err))(msgs.!)
                }

                val eok_ : v.eok = x => s_ => error => v.eok(x)(s_) {
                    if (errorIsUnknown(error)) error
                    else setExpectErrors(error)(msgs)
                }
                val eerr_ : v.eerr = err => v.eerr { setExpectErrors(err)(msgs) }
                unParser(p)(v.state)(v.cok)(v.cerr)(eok_)(eerr_)
            }
        } }

        // Token
        //
        def tokens[s, u, t](showTokens: List[t] => String_)
            (nextposs: SourcePos => List[t] => SourcePos)
            (tts: List[t])
            (implicit i: Stream[s, n, t], j: Eq[t]): _ParsecT[s, u, List[t]] = tts match
        {
            case Nil => _ParsecT { new UnParser[s, u, n, List[t]] {
                override def apply[b](v: UnParserParam[s, u, n, List[t], b]): n[b] = v.eok(Nil)(v.state) { unknownError(v.state) }
            } }
            case tts @ (tok :: toks) => _ParsecT { new UnParser[s, u, n, List[t]] {
                override def apply[b](v: UnParserParam[s, u, n, List[t], b]): n[b] = v.state match {
                    case state @ State(input, pos, u) => {
                        val errEof = setErrorMessage(Expect(showTokens(tts)))(newErrorMessage(SysUnExpect(""))(pos))
                        def errExpect(x: t) = setErrorMessage(Expect(showTokens(tts)))(newErrorMessage(SysUnExpect(showTokens(List(x))))(pos))

                        def ok(rs: s): n[b] = {
                            val pos_ = nextposs(pos)(tts)
                            val s_ = State(rs, pos_, u)
                            v.cok(tts)(s_)(newErrorUnknown(pos_))
                        }

                        import inner.`for`
                        import j.===

                        def walk(ts: List[t])(rs: s): n[b] = (ts, rs) match {
                            case (Nil, rs) => ok(rs)
                            case (t :: ts, rs) => for {
                                sr <- i.uncons(rs)
                                * <- sr match {
                                    case Nothing => v.cerr { errEof }
                                    case Just((x, xs)) => {
                                        if (t === x) walk(ts)(xs)
                                        else v.cerr { errExpect(x) }
                                    }
                                }
                            } yield *
                        }

                        for {
                            sr <- i.uncons(input)
                            * <- sr match {
                                case Nothing => v.eerr { errEof }
                                case Just((x, xs)) => {
                                    if (tok === x) walk(toks)(xs)
                                    else v.eerr { errExpect(x) }
                                }
                            }
                        } yield *
                    }
                }
            } }
        }

        def `try`[s, u, a](p: _ParsecT[s, u, a]): _ParsecT[s, u, a] = _ParsecT { new UnParser[s, u, n, a] {
            override def apply[b](v: UnParserParam[s, u, n, a, b]): n[b] = v.state match {
                case s @ State(_, pos, _) => {
                    val pcerr: v.cerr = parseError => v.eerr { setErrorPos(pos)(parseError) }
                    unParser(p)(s)(v.cok)(pcerr)(v.eok)(v.eerr)
                }
            }
        } }

        // Better evidence possible?
        def token[s, u, a, t](showToken: t => String_)
            (tokpos: t => SourcePos)
            (test: t => Maybe[a])(implicit i: Stream[s, n, t], ev: n[Maybe[(t, s)]] =:= Maybe[(t, s)]): UnParser[s, u, n, a] =
        {
            def nextpos(* : SourcePos)(tok: t)(ts: s): SourcePos = ev(i.uncons(ts)) match {
                case Nothing => tokpos(tok)
                case Just((tok_, _)) => tokpos(tok_)
            }
            tokenPrim(showToken)(nextpos)(test).get
        }

        def tokenPrim[s, u, a, t](showToken: t => String_)
            (nextpos: SourcePos => t => s => SourcePos)
            (test: t => Maybe[a])
            (implicit i: Stream[s, n, t]): _ParsecT[s, u, a] =
        {
            tokenPrimEx(showToken)(nextpos)(Nothing)(test)(i)
        }

        def tokenPrimEx[s, u, a, t](showToken: t => String_)
            (nextpos: SourcePos => t => s => SourcePos)
            (nextstate: Maybe[SourcePos => t => s => u => u])
            (test: t => Maybe[a])
            (implicit i: Stream[s, n, t]): _ParsecT[s, u, a] =
        {
            import inner.`for`

            nextstate match {
                case Nothing => _ParsecT { new UnParser[s, u, n, a] {
                    override def apply[b](v: UnParserParam[s, u, n, a, b]): n[b] = v.state match {
                        case State(input, pos, user) => for {
                            r <- i.uncons(input)
                            * <- r match {
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
                        } yield *
                    }
                } }
                case Just(nextState) => _ParsecT { new UnParser[s, u, n, a] {
                    override def apply[b](v: UnParserParam[s, u, n, a, b]): n[b] = v.state match {
                        case State(input, pos, user) => for {
                            r <- i.uncons(input)
                            * <- r match {
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
                        } yield *
                    }
                } }
            }
        }

        // Star
        //
        def many[s, u, a](p: _ParsecT[s, u, a]): _ParsecT[s, u, List[a]] = {
            val i = Monad[_ParsecT.apply[s, u]]
            import i.`for`
            for { xs <- manyAccum(List.op_::[a])(p) } yield List.reverse(xs)
        }

        def skipMany[s, u](p: _ParsecT[s, u, _]): _ParsecT[s, u, Unit] = {
            val i = Monad[_ParsecT.apply[s, u]]
            import i.`for`
            for { _ <- manyAccum[s, u, Any](_ => _ => Nil)(p) } yield ()
        }

        def manyAccum[s, u, a](acc: a => Lazy[List[a]] => List[a])(p: _ParsecT[s, u, a]): _ParsecT[s, u, List[a]] = _ParsecT { new UnParser[s, u, n, List[a]] {
            override def apply[b](v: UnParserParam[s, u, n, List[a], b]): n[b] = {
                def walk(xs: List[a])(x: a)(s_ : State[s, u])(err: ParseError): n[b] = {
                    unParser(p)(s_)(walk { acc(x)(xs) })(v.cerr)(manyErr)(e => v.cok(acc(x)(xs))(s_)(e))
                }
                unParser(p)(v.state)(walk(Nil))(v.cerr)(manyErr)(e => v.eok(Nil)(v.state)(e))
            }
        } }

        def manyErr: Nothing = error("ParsecT.many: combinator 'many' is applied to a parser that accepts an empty string.")

        // Run
        //   (runParserT is equivalent to runParser by the power of Scala)
        //
        def runParser[s, u, a](p: _ParsecT[s, u, a])(u: u)(name: SourceName)(s: s): n[Either[ParseError, a]] = {
            def parserReply(res: Consumed_[n[Reply[s, u, a]]]): n[Reply[s, u, a]] = res match {
                case Consumed(r) => r
                case Empty(r) => r
            }

            import inner._
            for {
                res <- runParsecT(p)(State(s, initialPos(name), u))
                r <- parserReply(res)
                * <- r match {
                    case Ok(x, _, _) => `return`(Right(x))
                    case Error(err) => `return`(Left(err))
                }
            } yield *
        }

        // Parse
        //
        def parse[s, u, a](p: _ParsecT[s, Unit, a])(name: SourceName)(s: s): n[Either[ParseError, a]] = runParser(p)(())(name)(s)

        def parseTest[s, u, a](p: _ParsecT[s, Unit, a])(input: s): IO[Unit] = {
            import IO.`for`
            parse(p)("")(input) match {
                case Left(err) => for { _ <- IO.putStr("parse error at "); * <- IO.print(err) } yield *
                case Right(x) => IO.print(x)
            }
        }

        // Getter/Setter
        //
        def getPosition[s, u]: _ParsecT[s, u, SourcePos] = {
            val i = Monad[_ParsecT.apply[s, u]]
            import i.`for`
            for { state <- getParserState[s, u] } yield statePos(state)
        }

        def getInput[s, u]: _ParsecT[s, u, s] = {
            val i = Monad[_ParsecT.apply[s, u]]
            import i.`for`
            for { state <- getParserState[s, u] } yield stateInput(state)
        }

        def setPosition[s, u](pos: SourcePos): _ParsecT[s, u, Unit] = {
            val i = Monad[_ParsecT.apply[s, u]]
            import i.`for`
            for { _ <- updateParserState[s, u] {
                    case State(input, _, user) => State(input, pos, user)
                }
            } yield ()
        }

        def setInput[s, u](input: s, * : Type[u] = null): _ParsecT[s, u, Unit] = {
            val i = Monad[_ParsecT.apply[s, u]]
            import i.`for`
            for { _ <- updateParserState[s, u] {
                    case State(_, pos, user) => State(input, pos, user)
                }
            } yield ()
        }

        def getParserState[s, u]: _ParsecT[s, u, State[s, u]] = updateParserState(id)

        def setParserState[s, u](st: State[s, u]): _ParsecT[s, u, State[s, u]] = updateParserState(const(st))

        def updateParserState[s, u](f: State[s, u] => State[s, u]): _ParsecT[s, u, State[s, u]] = _ParsecT { new UnParser[s, u, n, State[s, u]] {
            override def apply[b](v: UnParserParam[s, u, n, State[s, u], b]): n[b] = {
                val s_ = f(v.state)
                v.eok(s_)(s_) { unknownError(s_) }
            }
        } }

        def getState[s, u]: _ParsecT[s, u, u] = {
            val i = Monad[_ParsecT.apply[s, u]]
            i.liftM[State[s, u], u](stateUser)(getParserState)
        }

        def putState[s, u](u: u): _ParsecT[s, u, Unit] = {
            val i = Monad[_ParsecT.apply[s, u]]
            import i.`for`
            for { _ <- updateParserState { (s: State[s, u]) => s.copy(user = u) } } yield ()
        }

        def modifyState[s, u](f: u => u, * : Type[s] = null): _ParsecT[s, u, Unit] = {
            val i = Monad[_ParsecT.apply[s, u]]
            import i.`for`
            for { _ <- updateParserState { (s: State[s, u]) => s.copy(user = f(stateUser(s))) } } yield ()
        }

        def setState[s, u](u: u): _ParsecT[s, u, Unit] = putState(u)
        def updateState[s, u](f: u => u, * : Type[s] = null): _ParsecT[s, u, Unit] = modifyState(f, *)

        // Combinator
        //
        def choice[s, u, a](ps: List[_ParsecT[s, u, a]]): _ParsecT[s, u, a] = {
            val i = MonadPlus[_ParsecT.apply[s, u]]
            import i._
            List.foldr[apply1[a], i.apply1[a]](op_<|>)(mzero)(ps)
        }

        def option[s, u, a](x: a)(p: _ParsecT[s, u, a]): _ParsecT[s, u, a] = {
            val i = MonadPlus[_ParsecT.apply[s, u]]
            import i.<|>
            p <|> i.`return`(x)
        }

        def optionMaybe[s, u, a](x: a)(p: _ParsecT[s, u, a]): _ParsecT[s, u, Maybe[a]] = {
            val i = Monad[_ParsecT.apply[s, u]]
            option[s, u, Maybe[a]](Nothing)(i.liftM(Just(_: a))(p))
        }

        def optional[s, u](p: _ParsecT[s, u, _]): _ParsecT[s, u, Unit] = {
            val i = MonadPlus[_ParsecT.apply[s, u]]
            import i.{<|>, `for`}
            ( for { _ <- p } yield () ) <|> i.`return`()
        }

        // open p close
        def between[s, u, a](open: _ParsecT[s, u, _])(close: _ParsecT[s, u, _])(p: _ParsecT[s, u, a]): _ParsecT[s, u, a] = {
            val i = Monad[_ParsecT.apply[s, u]]
            import i.`for`
            for { _ <- open; x <- p; _ <- close } yield x
        }

        // p+ (result abandoned)
        def skipMany1[s, u](p: _ParsecT[s, u, _]): _ParsecT[s, u, Unit] = {
            val i = Monad[_ParsecT.apply[s, u]]
            import i.`for`
            for { _ <- p; * <- skipMany(p) } yield *
        }

        // p+
        def many1[s, u, a](p: _ParsecT[s, u, a]): _ParsecT[s, u, List[a]] = {
            val i = Monad[_ParsecT.apply[s, u]]
            import i.`for`
            for { x <- p; xs <- many(p) } yield (x :: xs)
        }

        // p (sep p)
        def sepBy1[s, u, a](p: _ParsecT[s, u, a])(sep: _ParsecT[s, u, _]): _ParsecT[s, u, List[a]] = {
            val i = Monad[_ParsecT.apply[s, u]]
            import i.{>>, `for`}
            for { x <- p; xs <- many(sep >> p) } yield (x :: xs)
        }

        def sepBy[s, u, a](p: _ParsecT[s, u, a])(sep: _ParsecT[s, u, _]): _ParsecT[s, u, List[a]] = {
            val i = MonadPlus[_ParsecT.apply[s, u]]
            import i.<|>
            sepBy1(p)(sep) <|> i.`return`(Nil)
        }

        def sepEndBy1[s, u, a](p: _ParsecT[s, u, a])(sep: _ParsecT[s, u, _]): _ParsecT[s, u, List[a]] = {
            val i = MonadPlus[_ParsecT.apply[s, u]]
            import i.{<|>, `for`}
            for {
                x <- p
                * <- ( for { _ <- sep; xs <- sepEndBy(p)(sep) } yield (x :: xs) ) <|> i.`return`(List(x))
            } yield *
        }

        def sepEndBy[s, u, a](p: _ParsecT[s, u, a])(sep: _ParsecT[s, u, _]): _ParsecT[s, u, List[a]] = {
            val i = MonadPlus[_ParsecT.apply[s, u]]
            import i.<|>
            sepEndBy1(p)(sep) <|> i.`return`(Nil)
        }

        // (p sep)+
        def endBy1[s, u, a](p: _ParsecT[s, u, a])(sep: _ParsecT[s, u, _]): _ParsecT[s, u, List[a]] = {
            val i = Monad[_ParsecT.apply[s, u]]
            import i.`for`
            many1 { for { x <- p; _ <- sep } yield x }
        }

        // (p sep)*
        def endBy[s, u, a](p: _ParsecT[s, u, a])(sep: _ParsecT[s, u, _]): _ParsecT[s, u, List[a]] = {
            val i = Monad[_ParsecT.apply[s, u]]
            import i.`for`
            many { for { x <- p; _ <- sep } yield x }
        }

        // p{n}
        def count[s, u, a](n: Int)(p: _ParsecT[s, u, a]): _ParsecT[s, u, List[a]] = {
            val i = Monad[_ParsecT.apply[s, u]]
            if (n <= 0) i.`return`(Nil)
            else i.sequence(List.replicate(n)(p))
        }

        // folding with seed
        def chainr[s, u, a](p: _ParsecT[s, u, a])(op: _ParsecT[s, u, a => a => a])(x: a): _ParsecT[s, u, a] = {
            val i = MonadPlus[_ParsecT.apply[s, u]]
            import i.<|>
            chainr1(p)(op) <|> i.`return`(x)
        }

        // folding with seed
        def chainl[s, u, a](p: _ParsecT[s, u, a])(op: _ParsecT[s, u, a => a => a])(x: a): _ParsecT[s, u, a] = {
            val i = MonadPlus[_ParsecT.apply[s, u]]
            import i.<|>
            chainl1(p)(op) <|> i.`return`(x)
        }

        // folding without seed
        def chainr1[s, u, a](p: _ParsecT[s, u, a])(op: _ParsecT[s, u, a => a => a]): _ParsecT[s, u, a] = {
            val i = MonadPlus[_ParsecT.apply[s, u]]
            import i.{<|>, `for`}
            def rest(x: a): _ParsecT[s, u, a] = ( for { f <- op; y <- scan } yield f(x)(y) ) <|> i.`return`(x)
            lazy val scan: _ParsecT[s, u, a] = for { x <- p; * <- rest(x) } yield *
            scan
        }

        // folding without seed
        def chainl1[s, u, a](p: _ParsecT[s, u, a])(op: _ParsecT[s, u, a => a => a]): _ParsecT[s, u, a] = {
            val i = MonadPlus[_ParsecT.apply[s, u]]
            import i.{<|>, `for`}
            def rest(x: a): _ParsecT[s, u, a] = ( for { f <- op; y <- p; z <- rest(f(x)(y)) } yield z ) <|> i.`return`(x)
            for { x <- p; * <- rest(x) } yield *
        }

        def anyToken[s, u, t](implicit si: Stream[s, n, t], sj: Show[t]): _ParsecT[s, u, t] = {
            tokenPrim[s, u, t, t](sj.show)(pos => _tok => _toks => pos)(Just(_))
        }

        def eof[s, u, t](implicit si: Stream[s, n, t], sj: Show[t]): _ParsecT[s, u, Unit] = {
            notFollowedBy(anyToken[s, u, t]) <#> "end of input"
        }

        // negative lookahead
        def notFollowedBy[s, u, a](p: _ParsecT[s, u, a])(implicit sj: Show[a]): _ParsecT[s, u, Unit] = {
            val i = MonadPlus[_ParsecT.apply[s, u]]
            import i.{<|>, `for`}
            `try` {
                ( for { c <- p; _ <- unexpected[s, u](sj.show(c)) } yield () ) <|> i.`return`()
            }
        }

        // star-until
        def manyTill[s, u, a](p: _ParsecT[s, u, a])(end: _ParsecT[s, u, _]): _ParsecT[s, u, List[a]] = {
            lazy val scan: _ParsecT[s, u, List[a]] = {
                val i = MonadPlus[_ParsecT.apply[s, u]]
                import i.{`for`, <|>}
                ( for { _ <- end } yield Nil.of[a] ) <|> ( for { x <- p; xs <- scan } yield (x :: xs) )
            }
            scan
        }

        // positive lookahead
        def lookAhead[s, u, a](p: _ParsecT[s, u, a]): _ParsecT[s, u, a] = {
            val i = Monad[_ParsecT.apply[s, u]]
            import i.`for`
            for { state <- getParserState[s, u]; x <- p; _ <- setParserState(state) } yield x
        }

        def unexpected[s, u](msg: String_): _ParsecT[s, u, Nothing] = _ParsecT { new UnParser[s, u, n, Nothing] {
            override def apply[b](v: UnParserParam[s, u, n, Nothing, b]): n[b] = {
                v.eerr { newErrorMessage(UnExpect(msg))(statePos(v.state)) }
            }
        } }

        // Char
        //
        def oneOf[s, u](cs: String_)(implicit si: Stream[s, n, Char]): _ParsecT[s, u, Char] = satisfy(c => List.elem(c)(cs))
        def noneOf[s, u](cs: String_)(implicit si: Stream[s, n, Char]): _ParsecT[s, u, Char] = satisfy(c => not(List.elem(c)(cs)))

        def spaces[s, u](implicit si: Stream[s, n, Char]): _ParsecT[s, u, Unit] = skipMany(space[s, u]) <#> "white space"

        def space[s, u](implicit si: Stream[s, n, Char]): _ParsecT[s, u, Char] = satisfy(Char.isSpace) <#> "space"
        def newline[s, u](implicit si: Stream[s, n, Char]): _ParsecT[s, u, Char] = char('\n') <#> "new-line"
        def tab[s, u](implicit si: Stream[s, n, Char]): _ParsecT[s, u, Char] = char('\t') <#> "tab"

        def upper[s, u](implicit si: Stream[s, n, Char]): _ParsecT[s, u, Char] = satisfy(Char.isUpper) <#> "uppercase letter"
        def lower[s, u](implicit si: Stream[s, n, Char]): _ParsecT[s, u, Char] = satisfy(Char.isLower) <#> "lowercase letter"
        def alphaNum[s, u](implicit si: Stream[s, n, Char]): _ParsecT[s, u, Char] = satisfy(Char.isAlphaNum) <#> "letter or digit"
        def letter[s, u](implicit si: Stream[s, n, Char]): _ParsecT[s, u, Char] = satisfy(Char.isAlpha) <#> "letter"
        def digit[s, u](implicit si: Stream[s, n, Char]): _ParsecT[s, u, Char] = satisfy(Char.isDigit) <#> "digit"
        def hexDigit[s, u](implicit si: Stream[s, n, Char]): _ParsecT[s, u, Char] = satisfy(Char.isHexDigit) <#> "hexadecimal digit"
        def octDigit[s, u](implicit si: Stream[s, n, Char]): _ParsecT[s, u, Char] = satisfy(Char.isOctDigit) <#> "octal digit"

        def char[s, u](c: Char)(implicit si: Stream[s, n, Char]): _ParsecT[s, u, Char] = satisfy(_ == c) <#> show(List(c))

        def anyChar[s, u](implicit si: Stream[s, n, Char]): _ParsecT[s, u, Char] = satisfy(const(True))

        def satisfy[s, u](f: Char => Bool)(implicit si: Stream[s, n, Char]): _ParsecT[s, u, Char] = {
            tokenPrim[s, u, Char, Char](c => show(List(c)))(pos => c => _cs => updatePosChar(pos)(c))(c => if (f(c)) Just(c) else Nothing)
        }

        def string[s, u](s: String_)(implicit si: Stream[s, n, Char]): _ParsecT[s, u, String_] = tokens[s, u, Char](show)(updatePosString)(s)
    }

    // Instances
    //
    private[ken] trait _ParsecT_0 { this: _ParsecT.type =>
        implicit def _asMonadPlus[s, u]: MonadPlus[({type m[+a] = _ParsecT[s, u, a]})#m] = new MonadPlus[({type m[+a] = _ParsecT[s, u, a]})#m] {
            // Monad
            private[this] type m[+a] = _ParsecT[s, u, a]
            override def `return`[a](x: Lazy[a]): m[a] = parserReturn(x)
            override def op_>>=[a, b](p: m[a])(f: a => m[b]): m[b] = parserBind(p)(f)
            // MonadPlus
            override def mzero: m[Nothing] = parserZero
            override def mplus[a](p1: m[a])(p2: Lazy[m[a]]): m[a] = parserPlus(p1)(p2)
        }

        implicit def _asMonadTrans[s, u]: MonadTrans[n, ({type m[+a] = _ParsecT[s, u, a]})#m] = new MonadTrans[n, ({type m[+a] = _ParsecT[s, u, a]})#m] {
            private[this] type m[+a] = _ParsecT[s, u, a]
            override def lift[a](amb: n[a]): m[a] = _ParsecT { new UnParser[s, u, n, a] {
                override def apply[b](v: UnParserParam[s, u, n, a, b]): n[b] = {
                    import inner.`for`
                    for {
                        a <- amb
                        * <- v.eok(a)(v.state) { unknownError(v.state) }
                    } yield *
                }
            } }
        }
    }

    private[ken] trait _ParsecT_1 extends _ParsecT_0 { this: _ParsecT.type =>
        implicit def _asMonadIO[s, u](implicit i: MonadIO[n]): MonadIO[({type m[+a] = _ParsecT[s, u, a]})#m] = new MonadIO[({type m[+a] = _ParsecT[s, u, a]})#m] with MonadProxy[({type m[+a] = _ParsecT[s, u, a]})#m] {
            private[this] type m[+a] = _ParsecT[s, u, a]
            override val selfMonad = _asMonadPlus[s, u]
            override def liftIO[a](io: IO[a]): m[a] = _asMonadTrans.lift(i.liftIO(io))
        }
    }

    private[ken] trait _ParsecT_2 extends _ParsecT_1 { this: _ParsecT.type =>
        implicit def _asMonadReader[s, u, r](implicit i: MonadReader[r, n]): MonadReader[r, ({type m[+a] = _ParsecT[s, u, a]})#m] = new MonadReader[r, ({type m[+a] = _ParsecT[s, u, a]})#m] with MonadProxy[({type m[+a] = _ParsecT[s, u, a]})#m] {
            private[this] type m[+a] = _ParsecT[s, u, a]
            override val selfMonad = _asMonadPlus[s, u]
            override def ask: m[r] = _asMonadTrans.lift(i.ask)
            override def local[a](f: r => r)(p: m[a]): m[a] = mkPT { s => i.local(f)(runParsecT(p)(s)) }
        }
    }

    private[ken] trait _ParsecT_3 extends _ParsecT_2 { this: _ParsecT.type =>
        implicit def _asMonadState[s, u, s_](implicit i: MonadState[s_, n]): MonadState[s_, ({type m[+a] = _ParsecT[s, u, a]})#m] = new MonadState[s_, ({type m[+a] = _ParsecT[s, u, a]})#m] with MonadProxy[({type m[+a] = _ParsecT[s, u, a]})#m] {
            private[this] type m[+a] = _ParsecT[s, u, a]
            override val selfMonad = _asMonadPlus[s, u]
            override def get: m[s_] = _asMonadTrans.lift(i.get)
            override def put(s: s_): m[Unit] = _asMonadTrans.lift(i.put(s))
        }
    }

    private[ken] trait _ParsecT_4 extends _ParsecT_3 { this: _ParsecT.type =>
        implicit def _asMonadCont[s, u](implicit i: MonadCont[n]): MonadCont[({type m[+a] = _ParsecT[s, u, a]})#m] = new MonadCont[({type m[+a] = _ParsecT[s, u, a]})#m] with MonadProxy[({type m[+a] = _ParsecT[s, u, a]})#m] {
            private[this] type m[+a] = _ParsecT[s, u, a]
            override val selfMonad = _asMonadPlus[s, u]
            override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = mkPT { s =>
                def pack(s: State[s, u])(a: a): Consumed_[n[Reply[s, u, a]]] = Empty(inner.`return`(Ok(a, s, unknownError(s)).up)).up
                i.callCC { (c: Consumed_[n[Reply[s, u, a]]] => n[Consumed_[n[Reply[s, u, b]]]]) =>
                    runParsecT( f( a => mkPT { s_ => c(pack(s_)(a)) } ) )(s)
                }
            }
        }
    }

    private[ken] trait _ParsecT_ extends _ParsecT_4 { this: _ParsecT.type =>
        implicit def _asMonadError[s, u, e](implicit i: MonadError[e, n]): MonadError[e, ({type m[+a] = _ParsecT[s, u, a]})#m] = new MonadError[e, ({type m[+a] = _ParsecT[s, u, a]})#m] with MonadProxy[({type m[+a] = _ParsecT[s, u, a]})#m] {
            private[this] type m[+a] = _ParsecT[s, u, a]
            override val selfMonad = _asMonadPlus[s, u]
            override def errorClass: ErrorClass[e] = i.errorClass
            override def throwError[a](e: e): m[a] = _asMonadTrans.lift(i.throwError(e))
            override def catchError[a](p: m[a])(h: e => m[a]): m[a] = mkPT { s =>
                i.catchError(runParsecT(p)(s)) { e =>
                    runParsecT(h(e))(s)
                }
            }
        }
    }
}
