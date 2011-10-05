

// Copyright Shunsuke Sogame 2011.
//
// (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package parsec


final case class ParsecT[s, u, n[+_], +a](override val get: UnParser[s, u, n, a]) extends NewtypeOf[UnParser[s, u, n, a]] with KindParsecT {
    override type stream = s
    override type userState = u
    override type innerMonad[+a] = n[a]

    def apply[b](v: UnParserParam[s, u, n, a, b]): n[b] = get(v)

    final def <#>(msg: String)(implicit i: Monad[n]): ParsecT[s, u, n, a] = ParsecTOp[ParsecT.apply3[s, u, n]].label(this)(msg)
}


object ParsecT extends ParsecTAs {
    trait apply2[s, u] extends Kind.MonadTransX {
        override type monadTrans[n[+_], +a] = ParsecT[s, u, n, a]
    }

    trait apply3[s, u, n[+_]] extends KindParsecT {
        override type apply1[+a] = ParsecT[s, u, n, a]
        override type oldtype1[+a] = UnParser[s, u, n, a]
        override type stream = s
        override type userState = u
        override type innerMonad[+a] = n[a]
    }
}


private[parsec] sealed trait ParsecTAs0 { this: ParsecT.type =>
    implicit def _asMonadTrans[s, u]: MonadTransX[apply2[s, u]#monadTrans] = new MonadTransX[apply2[s, u]#monadTrans] {
        private type t[n[+_], +a] = ParsecT[s, u, n, a]
        override def lift[n[+_], a](amb: n[a])(implicit i: Monad[n]): t[n, a] = ParsecT { new UnParser[s, u, n, a] {
            override def apply[b](v: UnParserParam[s, u, n, a, b]): n[b] = {
                import i.`for`
                for {
                    a <- amb
                    * <- v.eok(a)(v.state) { unknownError(v.state) }
                } yield *
            }
        } }
    }

    implicit def _asMonadIO[s, u, n[+_]](implicit i: MonadIO[n]): MonadIO[apply3[s, u, n]#apply1] = new MonadIO[apply3[s, u, n]#apply1] with MonadProxy[apply3[s, u, n]#apply1] {
        private type m[+a] = ParsecT[s, u, n, a]
        override def selfMonad = _asMonadPlus[s, u, n]
        override def liftIO[a](io: IO[a]): m[a] = _asMonadTrans[s, u].lift(i.liftIO(io))
    }

    implicit def _asMonadReader[s, u, n[+_], r](implicit i: MonadReader[r, n]): MonadReader[r, apply3[s, u, n]#apply1] = new MonadReader[r, apply3[s, u, n]#apply1] with MonadProxy[apply3[s, u, n]#apply1] {
        private type m[+a] = ParsecT[s, u, n, a]
        private val prim = ParsecTOp[apply3[s, u, n]]
        override def selfMonad = _asMonadPlus
        override def ask: m[r] = _asMonadTrans.lift(i.ask)
        override def local[a](f: r => r)(p: m[a]): m[a] = prim.mkPT { s => i.local(f)(prim.runParsecT(p)(s)) }
    }

    implicit def _asMonadState[s, u, n[+_], s_](implicit i: MonadState[s_, n]): MonadState[s_, apply3[s, u, n]#apply1] = new MonadState[s_, apply3[s, u, n]#apply1] with MonadProxy[apply3[s, u, n]#apply1] {
        private type m[+a] = ParsecT[s, u, n, a]
        override def selfMonad = _asMonadPlus[s, u, n]
        override def get: m[s_] = _asMonadTrans.lift(i.get)
        override def put(s: s_): m[Unit] = _asMonadTrans[s, u].lift(i.put(s))
    }

    implicit def _asMonadCont[s, u, n[+_]](implicit i: MonadCont[n]): MonadCont[apply3[s, u, n]#apply1] = new MonadCont[apply3[s, u, n]#apply1] with MonadProxy[apply3[s, u, n]#apply1] {
        private type m[+a] = ParsecT[s, u, n, a]
        private val prim = ParsecTOp[apply3[s, u, n]]
        override def selfMonad = _asMonadPlus[s, u, n]
        override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = prim.mkPT { s =>
            def pack(s: State[s, u])(a: a): Consumed_[n[Reply[s, u, a]]] = Empty(i.`return`(Ok(a, s, unknownError(s)).up)).up
            i.callCC { (c: Consumed_[n[Reply[s, u, a]]] => n[Consumed_[n[Reply[s, u, b]]]]) =>
                prim.runParsecT( f( a => prim.mkPT { s_ => c(pack(s_)(a)) } ) )(s)
            }
        }
    }

    implicit def _asMonadError[s, u, n[+_], e](implicit i: MonadError[e, n]): MonadError[e, apply3[s, u, n]#apply1] = new MonadError[e, apply3[s, u, n]#apply1] with MonadProxy[apply3[s, u, n]#apply1] {
        private type m[+a] = ParsecT[s, u, n, a]
        private val prim = ParsecTOp[apply3[s, u, n]]
        override def selfMonad = _asMonadPlus[s, u, n]
        override def throwError[a](e: e): m[a] = _asMonadTrans[s, u].lift(i.throwError(e))
        override def catchError[a](p: m[a])(h: e => m[a]): m[a] = prim.mkPT { s =>
            i.catchError(prim.runParsecT(p)(s)) { e =>
                prim.runParsecT(h(e))(s)
            }
        }
    }
}

private[parsec] sealed trait ParsecTAs extends ParsecTAs0 { this: ParsecT.type =>
    implicit def _asMonadPlus[s, u, n[+_]](implicit i: Monad[n]): MonadPlus[apply3[s, u, n]#apply1] = new MonadPlus[apply3[s, u, n]#apply1] {
        // Overrides
        //
        // Monad
        private type m[+a] = ParsecT[s, u, n, a]
        private val prim = ParsecTOp[apply3[s, u, n]]
        override def `return`[a](x: Lazy[a]): m[a] = prim.parserReturn(x)
        override def op_>>=[a, b](p: m[a])(f: a => m[b]): m[b] = prim.parserBind(p)(f)
        // MonadPlus
        override def mzero: m[Nothing] = prim.parserZero
        override def mplus[a](p1: m[a])(p2: Lazy[m[a]]): m[a] = prim.parserPlus(p1)(p2)
    }
}
