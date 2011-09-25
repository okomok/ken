

// Copyright Shunsuke Sogame 2011.
//
// (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package parsec


final class ParsecTs[s, u, n <: Kind.Function1](override implicit val inner: Monad[n#apply]) extends ParsecTsBase[s, u, n]

trait ParsecTsBase[s, u, n <: Kind.Function1] extends _ParsecTs[s, u, n#apply]


private[parsec] trait _ParsecTs[s, u, n[+_]] extends MonadTs[n]
    with _Prim[s, u, n] with _Combinators[s, u, n] with _Char[s, u, n]
{
    // ParsecT
    //
    final case class ParsecT[+a](override val get: UnParser[s, u, n, a]) extends NewtypeOf[UnParser[s, u, n, a]] {
        def apply[b](v: UnParserParam[s, u, n, a, b]): n[b] = get(v)

        final def <#>(msg: String): ParsecT[a] = label(this)(msg)
    }

    object ParsecT extends ParsecT_ with MonadPlus[ParsecT] with Kind.MonadTrans {
        override type oldtype1[+a] = UnParser[s, u, n, a]
        override type innerMonad[+a] = n[a]

        // Overrides
        //
        // Monad
        private type m[+a] = ParsecT[a]
        override def `return`[a](x: Lazy[a]): m[a] = parserReturn(x)
        override def op_>>=[a, b](p: m[a])(f: a => m[b]): m[b] = parserBind(p)(f)
        // MonadPlus
        override def mzero: m[Nothing] = parserZero
        override def mplus[a](p1: m[a])(p2: Lazy[m[a]]): m[a] = parserPlus(p1)(p2)

        implicit def dependent[a](n: NewtypeOf[UnParser[s, u, n, a]]): ParsecT[a] = ParsecT { n.get }
    }

    // Instances
    //
    private[parsec] trait ParsecT_0 { this: ParsecT.type =>
        implicit val _asMonadPlus: MonadPlus[ParsecT] = this

        implicit val _asMonadTrans: MonadTrans[n, ParsecT] = new MonadTrans[n, ParsecT] {
            private type m[+a] = ParsecT[a]
            override def lift[a](amb: n[a]): m[a] = ParsecT { new UnParser[s, u, n, a] {
                override def apply[b](v: UnParserParam[s, u, n, a, b]): n[b] = {
                    for {
                        a <- amb
                        * <- v.eok(a)(v.state) { unknownError(v.state) }
                    } yield *
                }
            } }
        }
    }

    private[parsec] trait ParsecT_1 extends ParsecT_0 { this: ParsecT.type =>
        implicit def _asMonadIO(implicit i: MonadIO[n]): MonadIO[ParsecT] = new MonadIO[ParsecT] with MonadProxy[ParsecT] {
            private type m[+a] = ParsecT[a]
            override def selfMonad = _asMonadPlus
            override def liftIO[a](io: IO[a]): m[a] = _asMonadTrans.lift(i.liftIO(io))
        }
    }

    private[parsec] trait ParsecT_2 extends ParsecT_1 { this: ParsecT.type =>
        implicit def _asMonadReader[r](implicit i: MonadReader[r, n]): MonadReader[r, ParsecT] = new MonadReader[r, ParsecT] with MonadProxy[ParsecT] {
            private type m[+a] = ParsecT[a]
            override def selfMonad = _asMonadPlus
            override def ask: m[r] = _asMonadTrans.lift(i.ask)
            override def local[a](f: r => r)(p: m[a]): m[a] = mkPT { s => i.local(f)(runParsecT(p)(s)) }
        }
    }

    private[parsec] trait ParsecT_3 extends ParsecT_2 { this: ParsecT.type =>
        implicit def _asMonadState[s_](implicit i: MonadState[s_, n]): MonadState[s_, ParsecT] = new MonadState[s_, ParsecT] with MonadProxy[ParsecT] {
            private type m[+a] = ParsecT[a]
            override def selfMonad = _asMonadPlus
            override def get: m[s_] = _asMonadTrans.lift(i.get)
            override def put(s: s_): m[Unit] = _asMonadTrans.lift(i.put(s))
        }
    }

    private[parsec] trait ParsecT_4 extends ParsecT_3 { this: ParsecT.type =>
        implicit def _asMonadCont(implicit i: MonadCont[n]): MonadCont[ParsecT] = new MonadCont[ParsecT] with MonadProxy[ParsecT] {
            private type m[+a] = ParsecT[a]
            override def selfMonad = _asMonadPlus
            override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = mkPT { s =>
                def pack(s: State[s, u])(a: a): Consumed_[n[Reply[s, u, a]]] = Empty(inner.`return`(Ok(a, s, unknownError(s)).up)).up
                i.callCC { (c: Consumed_[n[Reply[s, u, a]]] => n[Consumed_[n[Reply[s, u, b]]]]) =>
                    runParsecT( f( a => mkPT { s_ => c(pack(s_)(a)) } ) )(s)
                }
            }
        }
    }

    private[parsec] trait ParsecT_ extends ParsecT_4 { this: ParsecT.type =>
        implicit def _asMonadError[e](implicit i: MonadError[e, n]): MonadError[e, ParsecT] = new MonadError[e, ParsecT] with MonadProxy[ParsecT] {
            private type m[+a] = ParsecT[a]
            override def selfMonad = _asMonadPlus
            override def throwError[a](e: e): m[a] = _asMonadTrans.lift(i.throwError(e))
            override def catchError[a](p: m[a])(h: e => m[a]): m[a] = mkPT { s =>
                i.catchError(runParsecT(p)(s)) { e =>
                    runParsecT(h(e))(s)
                }
            }
        }
    }
}
