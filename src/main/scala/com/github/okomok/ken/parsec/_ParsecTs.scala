

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
        def accept[b](s: State[s, u])(v: _Visitor[s, u, a, b]): n[b]
    }

    trait _Visitor[s, u, -a, +b] {
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

    trait _VisitorProxy[s, u, -a, +b] extends Proxy with _Visitor[s, u, a, b] {
        override def self: _Visitor[s, u, a, b]
        override def cok: cok = self.cok
        override def cerr: cerr = self.cerr
        override def eok: eok = self.eok
        override def eerr: eerr = self.eerr
    }

    object _ParsecT extends _ParsecT_ with Kind.FunctionLike {
        sealed trait apply[s, u] extends Kind.AbstractMonadTrans {
            override type apply1[+a] = _ParsecT[s, u, a]
            override type oldtype1[+a] = Nothing
            override type innerMonad[+a] = n[a]
        }

        def unParser[s, u, a, b](p: _ParsecT[s, u, a])(s: State[s, u])
            (cok_ : a => State[s, u] => ParseError => n[b])
            (cerr_ : ParseError => n[b])
            (eok_ : a => State[s, u] => ParseError => n[b])
            (eerr_ : ParseError => n[b]): n[b] =
        {
            p.accept(s) {
                new _Visitor[s, u, a, b] {
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
                new _Visitor[s, u, a, Consumed_[n[Reply[s, u, a]]]] {
                    override val cok: cok = a => s_ => err => `return`(Consumed(`return`(Ok(a, s_, err).up)).up)
                    override val cerr: cerr = err => `return`(Consumed(`return`(Error(err).up)).up)
                    override val eok: eok = a => s_ => err => `return`(Empty(`return`(Ok(a, s_, err).up)).up)
                    override val eerr: eerr = err => `return`(Empty(`return`(Error(err).up)).up)
                }
            }
        }

        def mkPT[s, u, a](k: State[s, u] => n[Consumed_[n[Reply[s, u, a]]]]): _ParsecT[s, u, a] = new _ParsecT[s, u, a] {
            override def accept[b](s: State[s, u])(v: _Visitor[s, u, a, b]): n[b] = {
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
            override def accept[b](s: State[s, u])(v: _Visitor[s, u, a, b]): n[b] = v.eok(x)(s)(unknownError(s))
        }

        // sequence
        def parserBind[s, u, a, z](m: _ParsecT[s, u, a])(k: a => _ParsecT[s, u, z]): _ParsecT[s, u, z] = new _ParsecT[s, u, z] {
            override def accept[b](s: State[s, u])(v: _Visitor[s, u, z, b]): n[b] = {
                m.accept(s) {
                    new _Visitor[s, u, a, b] {
                        override val cok: cok = x => s => err => {
                            k(x).accept(s) {
                                new _Visitor[s, u, z, b] {
                                    override val cok = v.cok
                                    override val cerr = v.cerr
                                    override val eok: eok = x => s => err_ => v.cok(x)(s)(mergeError(err)(err_))
                                    override val eerr: eerr = err_ => v.cerr(mergeError(err)(err_))
                                }
                            }
                        }
                        override val cerr = v.cerr
                        override val eok: eok = x => s => err => {
                            k(x).accept(s) {
                                new _Visitor[s, u, z, b] {
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
            override def accept[b](s: State[s, u])(v: _Visitor[s, u, Nothing, b]): n[b] = v.eerr { unknownError(s) }
        }

        // alternative
        def parserPlus[s, u, a](m: _ParsecT[s, u, a])(n: _ParsecT[s, u, a]): _ParsecT[s, u, a] = new _ParsecT[s, u, a] {
            override def accept[b](s: State[s, u])(v: _Visitor[s, u, a, b]): n[b] = {
                m.accept(s) {
                    new _VisitorProxy[s, u, a, b] {
                        override val self = v
                        override val eerr: eerr = err => {
                            n.accept(s) {
                                new _VisitorProxy[s, u, a, b] {
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
                override def accept[b](s: State[s, u])(v: _Visitor[s, u, a, b]): n[b] = {
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
