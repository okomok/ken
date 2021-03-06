

// Copyright Shunsuke Sogame 2011.
//
// (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package parsec


final case class ParsecT[s, u, n[+_], +a](override val old: UnParser[s, u, n, a]) extends NewtypeOf[UnParser[s, u, n, a]] with KindParsecT {
    override type stream = s
    override type userState = u
    override type innerMonad[+a] = n[a]

    def apply[b](v: UnParserParam[s, u, n, a, b]): n[b] = get(v)

    final def <#>(msg: String)(implicit i: Monad[n]): ParsecT[s, u, n, a] = ParsecTOp[ParsecT.apply3[s, u, Kind.quote1[n]]].label(this)(msg)
}


object ParsecT extends ParsecTAs {
    trait apply2[s, u] extends Kind.MonadTrans {
        override type monadTrans[n[+_], +a] = ParsecT[s, u, n, a]
    }

    trait apply3[s, u, n <: Kind.Function1] extends KindParsecT {
        override type apply1[+a] = ParsecT[s, u, n#apply1, a]
        override type oldtype1[+a] = UnParser[s, u, n#apply1, a]
        override type stream = s
        override type userState = u
        override type innerMonad[+a] = n#apply1[a]
    }
}


private[parsec] sealed trait ParsecTAs0 { this: ParsecT.type =>
    implicit def _asMonadTrans[s, u]: MonadTrans[({type L[n[+_], +a] = ParsecT[s, u, n, a]})#L] = new MonadTrans[({type L[n[+_], +a] = ParsecT[s, u, n, a]})#L] {
        private type t[n[+_], +a] = ParsecT[s, u, n, a]
        override def lift[n[+_], a](amb: n[a])(implicit i: Monad[n]): t[n, a] = ParsecT { new UnParser[s, u, n, a] {
            override def apply[b](v: UnParserParam[s, u, n, a, b]): n[b] = {
                import i.`for`
                for {
                    a <- amb
                } {
                    v.eok(a)(v.state) { unknownError(v.state) }
                }
            }
        } }
    }

    implicit def _asMonadIO[s, u, n[+_]](implicit i: MonadIO[n]): MonadIO[({type L[+a] = ParsecT[s, u, n, a]})#L] = new MonadIO[({type L[+a] = ParsecT[s, u, n, a]})#L] with MonadProxy[({type L[+a] = ParsecT[s, u, n, a]})#L] {
        private type m[+a] = ParsecT[s, u, n, a]
        override def selfMonad: selfMonad = _asMonadPlus[s, u, n]
        override def liftIO[a](io: IO[a]): m[a] = _asMonadTrans[s, u].lift(i.liftIO(io))
    }

    implicit def _asMonadReader[s, u, n[+_]](implicit i: MonadReader[n]): MonadReader.Of[i.ReadType, ({type L[+a] = ParsecT[s, u, n, a]})#L] = new MonadReader[({type L[+a] = ParsecT[s, u, n, a]})#L] with MonadProxy[({type L[+a] = ParsecT[s, u, n, a]})#L] {
        private type m[+a] = ParsecT[s, u, n, a]
        private val prim = ParsecTOp[apply3[s, u, Kind.quote1[n]]]
        override def selfMonad: selfMonad = _asMonadPlus
        override type ReadType = i.ReadType
        override val ask: ask = _asMonadTrans.lift(i.ask)
        override def local[a](f: ReadType => ReadType)(p: m[a]): m[a] = prim.mkPT { s => i.local(f)(prim.runParsecT(p)(s)) }
    }

    implicit def _asMonadState[s, u, n[+_]](implicit i: MonadState[n]): MonadState.Of[i.StateType, ({type L[+a] = ParsecT[s, u, n, a]})#L] = new MonadState[({type L[+a] = ParsecT[s, u, n, a]})#L] with MonadProxy[({type L[+a] = ParsecT[s, u, n, a]})#L] {
        private type m[+a] = ParsecT[s, u, n, a]
        override def selfMonad: selfMonad = _asMonadPlus[s, u, n]
        override type StateType = i.StateType
        override val get: get = _asMonadTrans.lift(i.get)
        override val put: put = s => _asMonadTrans[s, u].lift(i.put(s))
    }

    implicit def _asMonadCont[s, u, n[+_]](implicit i: MonadCont[n]): MonadCont[({type L[+a] = ParsecT[s, u, n, a]})#L] = new MonadCont[({type L[+a] = ParsecT[s, u, n, a]})#L] with MonadProxy[({type L[+a] = ParsecT[s, u, n, a]})#L] {
        private type m[+a] = ParsecT[s, u, n, a]
        private val prim = ParsecTOp[apply3[s, u, Kind.quote1[n]]]
        override def selfMonad: selfMonad = _asMonadPlus[s, u, n]
        override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = prim.mkPT { s =>
            def pack(s: State[s, u])(a: a): Consumed_[n[Reply[s, u, a]]] = Empty(i.`return`(Ok(a, s, unknownError(s)).up)).up
            i.callCC { (c: Consumed_[n[Reply[s, u, a]]] => n[Consumed_[n[Reply[s, u, b]]]]) =>
                prim.runParsecT( f( a => prim.mkPT { s_ => c(pack(s_)(a)) } ) )(s)
            }
        }
    }

    implicit def _asMonadError[s, u, n[+_]](implicit i: MonadError[n]): MonadError.Of[i.ErrorType, ({type L[+a] = ParsecT[s, u, n, a]})#L] = new MonadError[({type L[+a] = ParsecT[s, u, n, a]})#L] with MonadProxy[({type L[+a] = ParsecT[s, u, n, a]})#L] {
        private type m[+a] = ParsecT[s, u, n, a]
        private val prim = ParsecTOp[apply3[s, u, Kind.quote1[n]]]
        override def selfMonad: selfMonad = _asMonadPlus[s, u, n]
        override type ErrorType = i.ErrorType
        override val throwError: throwError = e => _asMonadTrans[s, u].lift(i.throwError(e))
        override def catchError[a](p: m[a])(h: ErrorType => m[a]): m[a] = prim.mkPT { s =>
            i.catchError(prim.runParsecT(p)(s)) { e =>
                prim.runParsecT(h(e))(s)
            }
        }
    }
}

private[parsec] sealed trait ParsecTAs extends ParsecTAs0 { this: ParsecT.type =>
    implicit def _asMonadPlus[s, u, n[+_]](implicit i: Monad[n]): MonadPlus[({type L[+a] = ParsecT[s, u, n, a]})#L] = new MonadPlus[({type L[+a] = ParsecT[s, u, n, a]})#L] {
        // Overrides
        //
        // Monad
        private type m[+a] = ParsecT[s, u, n, a]
        private val prim = ParsecTOp[apply3[s, u, Kind.quote1[n]]]
        override def `return`[a](x: Lazy[a]): m[a] = prim.parserReturn(x)
        override def op_>>=[a, b](p: m[a])(f: a => m[b]): m[b] = prim.parserBind(p)(f)
        // MonadPlus
        override def mzero: m[Nothing] = prim.parserZero
        override def mplus[a](p1: m[a])(p2: Lazy[m[a]]): m[a] = prim.parserPlus(p1)(p2)
    }
}
