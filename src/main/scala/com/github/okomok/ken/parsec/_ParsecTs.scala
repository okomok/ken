

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
        def apply[b](s: State[s, u])
            (cok: a => State[s, u] => ParseError => n[b])
            (cerr: ParseError => n[b])
            (eok: a => State[s, u] => ParseError => n[b])
            (eerr: ParseError => n[b]): n[b]
    }

    object _ParsecT extends _ParsecT_ with Kind.FunctionLike {
        sealed trait apply[s, u] extends Kind.AbstractMonadTrans {
            override type apply1[+a] = _ParsecT[s, u, a]
            override type oldtype1[+a] = Nothing
            override type innerMonad[+a] = n[a]
        }

        def run[s, u, a](p: _ParsecT[s, u, a])(s: State[s, u]): n[Consumed_[n[Reply[s, u, a]]]] = {
            import inner.`return`
            def cok(a: a)(s_ : State[s, u])(err: ParseError) = `return`(Consumed(`return`(Ok(a, s_, err).up)).up)
            def cerr(err: ParseError) = `return`(Consumed(`return`(Error(err).up)).up)
            def eok(a: a)(s_ : State[s, u])(err: ParseError) = `return`(Empty(`return`(Ok(a, s_, err).up)).up)
            def eerr(err: ParseError) = `return`(Empty(`return`(Error(err).up)).up)
            p(s)(cok)(cerr)(eok)(eerr)
        }

        def mkPT[s, u, a](k: State[s, u] => n[Consumed_[n[Reply[s, u, a]]]]): _ParsecT[s, u, a] = new _ParsecT[s, u, a] {
            override def apply[b](s: State[s, u])
                (cok: a => State[s, u] => ParseError => n[b])
                (cerr: ParseError => n[b])
                (eok: a => State[s, u] => ParseError => n[b])
                (eerr: ParseError => n[b]): n[b] =
            {
                import inner.forComp
                for {
                    cons <- k(s)
                    * <- cons match {
                        case Consumed(mrep) => for {
                            rep <- mrep
                            * <- rep match {
                                case Ok(x, s_, err) => cok(x)(s_)(err)
                                case Error(err) => cerr(err)
                            }
                        } yield *
                        case Empty(mrep) => for {
                            rep <- mrep
                            * <- rep match {
                                case Ok(x, s_, err) => eok(x)(s_)(err)
                                case Error(err) => eerr(err)
                            }
                        } yield *
                    }
                } yield *
            }
        }
    }

    private[ken] trait _ParsecT_ { this: _ParsecT.type =>

    }
}
