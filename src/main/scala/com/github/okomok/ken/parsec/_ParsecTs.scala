

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
    trait _ParsecT[s, u, +a] extends Kind.constThis {
        def accept[b](s: State[s, u])(v: _ParsecT.Visitor[s, u, a, b]): n[b]

        final def <#>(msg: String_): _ParsecT[s, u, a] = _ParsecT.label(this)(msg)
    }

    object _ParsecT extends _ParsecT_ with Kind.FunctionLike {
        sealed trait apply[s, u] extends Kind.AbstractMonadTrans {
            override type apply1[+a] = _ParsecT[s, u, a]
            override type oldtype1[+a] = Nothing
            override type innerMonad[+a] = n[a]
        }

        trait Visitor[s, u, -a, +b] {
            // type-ascription can avoid "missing parameter type".
            type cok = a => State[s, u] => ParseError => n[b]
            type cerr = ParseError => n[b]
            type eok = a => State[s, u] => ParseError => n[b]
            type eerr = ParseError => n[b]
            def cok: cok
            def cerr: cerr
            def eok: eok
            def eerr: eerr
        }

        trait VisitorProxy[s, u, -a, +b] extends Proxy with Visitor[s, u, a, b] {
            override def self: Visitor[s, u, a, b]
            override def cok: cok = self.cok
            override def cerr: cerr = self.cerr
            override def eok: eok = self.eok
            override def eerr: eerr = self.eerr
        }

        def unParser[s, u, a, b](p: _ParsecT[s, u, a])(s: State[s, u])
            (cok_ : a => State[s, u] => ParseError => n[b])
            (cerr_ : ParseError => n[b])
            (eok_ : a => State[s, u] => ParseError => n[b])
            (eerr_ : ParseError => n[b]): n[b] =
        {
            p.accept(s) {
                new Visitor[s, u, a, b] {
                    override val cok = cok_
                    override val cerr = cerr_
                    override val eok = eok_
                    override val eerr = eerr_
                }
            }
        }

        def runParsecT[s, u, a](p: _ParsecT[s, u, a])(s: State[s, u]): n[Consumed_[n[Reply[s, u, a]]]] = {
            import inner.`return`
            p.accept(s) {
                new Visitor[s, u, a, Consumed_[n[Reply[s, u, a]]]] {
                    override val cok: cok = a => s_ => err => `return`(Consumed(`return`(Ok(a, s_, err).up)).up)
                    override val cerr: cerr = err => `return`(Consumed(`return`(Error(err).up)).up)
                    override val eok: eok = a => s_ => err => `return`(Empty(`return`(Ok(a, s_, err).up)).up)
                    override val eerr: eerr = err => `return`(Empty(`return`(Error(err).up)).up)
                }
            }
        }

        def mkPT[s, u, a](k: State[s, u] => n[Consumed_[n[Reply[s, u, a]]]]): _ParsecT[s, u, a] = new _ParsecT[s, u, a] {
            override def accept[b](s: State[s, u])(v: Visitor[s, u, a, b]): n[b] = {
                import inner.`for`
                for {
                    cons <- k(s)
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
        }

        // epsilon
        def parserReturn[s, u, a, c](x: a): _ParsecT[s, u, a] = new _ParsecT[s, u, a] {
            override def accept[b](s: State[s, u])(v: Visitor[s, u, a, b]): n[b] = v.eok(x)(s)(unknownError(s))
        }

        // sequence
        def parserBind[s, u, a, z](m: _ParsecT[s, u, a])(k: a => _ParsecT[s, u, z]): _ParsecT[s, u, z] = new _ParsecT[s, u, z] {
            override def accept[b](s: State[s, u])(v: Visitor[s, u, z, b]): n[b] = {
                m.accept(s) {
                    new Visitor[s, u, a, b] {
                        override val cok: cok = x => s => err => {
                            val pcok = v.cok
                            val pcerr = v.cerr
                            val peok: v.eok = x => s => err_ => v.cok(x)(s)(mergeError(err)(err_))
                            val peerr: v.eerr = err_ => v.cerr(mergeError(err)(err_))
                            unParser(k(x))(s)(pcok)(pcerr)(peok)(peerr)
                            /*
                            k(x).accept(s) {
                                new Visitor[s, u, z, b] {
                                    override val cok = v.cok
                                    override val cerr = v.cerr
                                    override val eok: eok = x => s => err_ => v.cok(x)(s)(mergeError(err)(err_))
                                    override val eerr: eerr = err_ => v.cerr(mergeError(err)(err_))
                                }
                            }
                            */
                        }
                        override val cerr = v.cerr
                        override val eok: eok = x => s => err => {
                            k(x).accept(s) {
                                new Visitor[s, u, z, b] {
                                    override val cok = v.cok
                                    override val eok: eok = x => s => err_ => v.eok(x)(s)(mergeError(err)(err_))
                                    override val cerr = v.cerr
                                    override val eerr: eerr = err_ => v.eerr(mergeError(err)(err_))
                                }
                            }
                        }
                        override val eerr = v.eerr
                    }
                }
            }
        }

        // always-fail
        def parserZero[s, u]: _ParsecT[s, u, Nothing] = new _ParsecT[s, u, Nothing] {
            override def accept[b](s: State[s, u])(v: Visitor[s, u, Nothing, b]): n[b] = v.eerr { unknownError(s) }
        }

        // alternative
        def parserPlus[s, u, a](m: _ParsecT[s, u, a])(n: _ParsecT[s, u, a]): _ParsecT[s, u, a] = new _ParsecT[s, u, a] {
            override def accept[b](s: State[s, u])(v: Visitor[s, u, a, b]): n[b] = {
                m.accept(s) {
                    new VisitorProxy[s, u, a, b] {
                        override val self = v
                        override val eerr: eerr = err => {
                            n.accept(s) {
                                new VisitorProxy[s, u, a, b] {
                                    override val self = v
                                    override val eok: eok = y => s_ => err_ => eok(y)(s_)(mergeError(err)(err_))
                                    override val eerr: eerr = err_ => eerr { mergeError(err)(err_) }
                                }
                            }
                        }
                    }
                }
            }
        }

        def label[s, u, a](p: _ParsecT[s, u, a])(msg: String_): _ParsecT[s, u, a] = labels(p)(List(msg))

        def labels[s, u, a](p: _ParsecT[s, u, a])(msgs: List[String_]): _ParsecT[s, u, a] = new _ParsecT[s, u, a] {
            override def accept[b](s: State[s, u])(v: Visitor[s, u, a, b]): n[b] = {
                def setExpectErrors(err: ParseError)(msgs: List[String_]): ParseError = msgs match {
                    case Nil => setErrorMessage(Expect(""))(err)
                    case msg !:: Nil => setErrorMessage(Expect(msg))(err)
                    case msg :: msgs => List.foldr[String_, ParseError](msg => err => addErrorMessage(Expect(msg))(err))(setErrorMessage(Expect(msg))(err))(msgs.!)
                }

                p.accept(s) {
                    new VisitorProxy[s, u, a, b] {
                        override val self = v
                        override val eok: eok = x => s_ => error => v.eok(x)(s_) {
                            if (errorIsUnknown(error)) error
                            else setExpectErrors(error)(msgs)
                        }
                        override val eerr: eerr = err => v.eerr { setExpectErrors(err)(msgs) }
                    }
                }
            }
        }

        // Tokens
        //
        trait Stream[s, t] {
            def uncons: s => n[Maybe[(t, s)]]
        }

        def tokens[s, u, t](showTokens: List[t] => String_)
            (nextposs: SourcePos => List[t] => SourcePos)
            (tts: List[t])
            (implicit i: Stream[s, t], j: Eq[t]): _ParsecT[s, u, List[t]] = tts match
        {
            case Nil => new _ParsecT[s, u, List[t]] {
                override def accept[b](s: State[s, u])(v: Visitor[s, u, List[t], b]): n[b] = v.eok(Nil)(s) { unknownError(s) }
            }
            case tts @ (tok :: toks) =>  new _ParsecT[s, u, List[t]] {
                override def accept[b](s: State[s, u])(v: Visitor[s, u, List[t], b]): n[b] = s match {
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
            }
        }

        def `try`[s, u, a](p: _ParsecT[s, u, a]): _ParsecT[s, u, a] = new _ParsecT[s, u, a] {
            override def accept[b](s: State[s, u])(v: Visitor[s, u, a, b]): n[b] = s match {
                case s @ State(_, pos, _) => {
                    p.accept(s) {
                        new VisitorProxy[s, u, a, b] {
                            override val self = v
                            override val cerr: cerr = parseError => v.eerr { setErrorPos(pos)(parseError) }
                        }
                    }
                }
            }
        }

        // def token?

        def tokenPrim[s, u, a, t](showToken: t => String_)
            (nextpos: SourcePos => t => s => SourcePos)
            (test: t => Maybe[a])
            (implicit i: Stream[s, t]): _ParsecT[s, u, a] =
        {
            tokenPrimEx(showToken)(nextpos)(Nothing)(test)(i)
        }

        def tokenPrimEx[s, u, a, t](showToken: t => String_)
            (nextpos: SourcePos => t => s => SourcePos)
            (nextstate: Maybe[SourcePos => t => s => u => u])
            (test: t => Maybe[a])
            (implicit i: Stream[s, t]): _ParsecT[s, u, a] =
        {
            import inner.`for`

            nextstate match {
                case Nothing => new _ParsecT[s, u, a] {
                    override def accept[b](s: State[s, u])(v: Visitor[s, u, a, b]): n[b] = s match {
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
                }
                case Just(nextState) => new _ParsecT[s, u, a] {
                    override def accept[b](s: State[s, u])(v: Visitor[s, u, a, b]): n[b] = s match {
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
                }
            }
        }

        // star
        //
        def many[s, u, a](p: _ParsecT[s, u, a]): _ParsecT[s, u, List[a]] = {
            val i = Monad[_ParsecT.apply[s, u]]
            import i._
            for { xs <- manyAccum(List.op_::[a])(p) } yield List.reverse(xs)
        }

        def skipMany[s, u](p: _ParsecT[s, u, _]): _ParsecT[s, u, Unit] = {
            val i = Monad[_ParsecT.apply[s, u]]
            import i.`for`
            for { _ <- manyAccum[s, u, Any](_ => _ => Nil)(p) } yield ()
        }

        def manyAccum[s, u, a](acc: a => Lazy[List[a]] => List[a])(p: _ParsecT[s, u, a]): _ParsecT[s, u, List[a]] = new _ParsecT[s, u, List[a]] {
            override def accept[b](s: State[s, u])(v: Visitor[s, u, List[a], b]): n[b] = {
                def walk(xs: List[a])(x: a)(s_ : State[s, u])(err: ParseError): n[b] = {
                    unParser(p)(s_)(walk { acc(x)(xs) })(v.cerr)(manyErr)(e => v.cok(acc(x)(xs))(s_)(e))
                }
                unParser(p)(s)(walk(Nil))(v.cerr)(manyErr)(e => v.eok(Nil)(s)(e))
            }
        }

        def manyErr: Nothing = error("ParsecT.many: : combinator 'many' is applied to a parser that accepts an empty string.")

    }

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
            override def lift[a](amb: n[a]): m[a] = new _ParsecT[s, u, a] {
                override def accept[b](s: State[s, u])(v: Visitor[s, u, a, b]): n[b] = {
                    import inner.`for`
                    for {
                        a <- amb
                        * <- v.eok(a)(s) { unknownError(s) }
                    } yield *
                }
            }
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
