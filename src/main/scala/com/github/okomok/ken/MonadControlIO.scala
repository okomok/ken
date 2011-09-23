

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait MonadControlIO[m[+_]] extends MonadIO[m] {
    final val asMonadControlIO: MonadControlIO[apply] = this

    type RunInIO = MonadControlIO.RunInIO[m]

    // Core
    //
    def liftControlIO[a](f: RunInIO => IO[a]): m[a] = error("todo")

    // Extra
    //
    def controlIO[a](f: RunInIO => IO[m[a]]): m[a] = join(liftControlIO(f))

    def liftIOOp[a, b, c](f: (a => IO[m[b]]) => IO[m[c]]): (a => m[b]) => m[c] = g => controlIO { runInIO => f { a => runInIO(g(a)) } }
    def liftIOOp_[a, b](f: IO[m[a]] => IO[m[b]]): m[a] => m[b] = m => controlIO { runInIO => f { runInIO(m) } }

    // Exceptions
    //
    def `catch`[e, a](a: m[a])(handler: e => m[a])(implicit j: Exception[e]): m[a] = controlIO { runInIO =>
        j.`catch`(runInIO(a))(e => runInIO { handler(e) })
    }

    final case class Handler[a](rep: (e => m[a], Exception[e]) forSome { type e })

    object Handler {
        def apply[e, a](h: e => m[a])(implicit i: Exception[e]): Handler[a] = new Handler(h, i)
    }

    def catches[a](a: m[a])(handlers: List[Handler[a]]): m[a] = controlIO { runInIO =>
        Exception.catches(runInIO(a)) {
            for {
                h <- handlers
            } yield {
                def impl[e](x: (e => m[a], Exception[e])): Exception.Handler[m[a]] = {
                    Exception.Handler((e: e) => runInIO { x._1(e) })(x._2)
                }
                impl(h.rep)
            }
        }
    }

    def catchJust[e, a, b](p: e => Maybe[b])(a: m[a])(handler: b => m[a])(implicit j: Exception[e]): m[a] = controlIO { runInIO =>
        j.catchJust(p)(runInIO(a))(e => runInIO(handler(e)))
    }

    def handle[e, a](handler: e => m[a])(a: m[a])(implicit j: Exception[e]): m[a] = controlIO { runInIO =>
        j.handle(e => runInIO(handler(e)))(runInIO(a))
    }

    def handleJust[e, a, b](p: e => Maybe[b])(handler: b => m[a])(a: m[a])(implicit j: Exception[e]): m[a] = controlIO { runInIO =>
        j.handleJust(p)(e => runInIO(handler(e)))(runInIO(a))
    }

    def `try`[e, a](a: m[a])(implicit j: Exception[e]): m[Either[e, a]] = liftIOOp_((x: IO[m[a]]) => IO.liftM(sequenceEither[e, a])(j.`try`(x)))(a)

    def tryJust[e, a, b](p: e => Maybe[b])(a: m[a])(implicit j: Exception[e]): m[Either[b, a]] = liftIOOp_((x: IO[m[a]]) => IO.liftM(sequenceEither[b, a])(j.tryJust(p)(x)))(a)

    def bracket[a, b, c](before: m[a])(after: a => m[b])(thing: a => m[c]): m[c] = controlIO { runInIO =>
        Exception.bracket(runInIO(before))((m: m[a]) => runInIO { m >>= after })((m: m[a]) => runInIO { m >>= thing })
    }

    def bracket_[a, b, c](before: m[a])(after: m[b])(thing: m[c]): m[c] = controlIO { runInIO =>
        Exception.bracket_(runInIO(before))(runInIO(after))(runInIO(thing))
    }

    def bracketOnError[a, b, c](before: m[a])(after: a => m[b])(thing: a => m[c]): m[c] = controlIO { runInIO =>
        Exception.bracketOnError(runInIO(before))((m: m[a]) => runInIO { m >>= after })((m: m[a]) => runInIO { m >>= thing })
    }

    def `finally`[a, b](a: m[a])(sequel: m[b]): m[a] = controlIO { runInIO =>
        Exception.`finally`(runInIO(a))(runInIO(sequel))
    }

    def onException[a, b](m: m[a])(what: m[b]): m[a] = controlIO { runInIO =>
        Exception.onException(runInIO(m))(runInIO(what))
    }
}


trait MonadControlIOProxy[m[+_]] extends MonadControlIO[m] with MonadIOProxy[m] {
    def selfMonadControlIO: MonadControlIO[m]
    override def selfMonadIO: MonadIO[m] = selfMonadControlIO

    override def liftControlIO[a](f: RunInIO => IO[a]): m[a] = selfMonadControlIO.liftControlIO(f)

    override def controlIO[a](f: RunInIO => IO[m[a]]): m[a] = selfMonadControlIO.controlIO(f)
    override def liftIOOp[a, b, c](f: (a => IO[m[b]]) => IO[m[c]]): (a => m[b]) => m[c] = selfMonadControlIO.liftIOOp(f)
    override def liftIOOp_[a, b](f: IO[m[a]] => IO[m[b]]): m[a] => m[b] = selfMonadControlIO.liftIOOp_(f)
}


object MonadControlIO {
    def apply[m <: Kind.Function1](implicit i: MonadControlIO[m#apply]): MonadControlIO[m#apply] = i

    def deriving[nt <: Kind.Function1, ot <: Kind.Function1](implicit i: MonadControlIO[ot#apply], j: Newtype1[nt#apply, ot#apply]): MonadControlIO[nt#apply] = new MonadControlIO[nt#apply] with MonadIOProxy[nt#apply] {
        private type m[+a] = nt#apply[a]
        override val selfMonadIO = MonadIO.deriving[nt, ot]

        override def liftIO[a](io: IO[a]): m[a] = j.newOf { i.liftIO(io) }
        override def liftControlIO[a](f: RunInIO => IO[a]): m[a] = j.newOf {
            i.liftControlIO { run =>
                f {
                    new RunInIO {
                        override def apply[b](t: m[b]): IO[m[b]] = for { x <- run(j.oldOf(t)) } yield j.newOf(x)
                    }
                }
            }
        }
    }

    def weak[nt <: Kind.Newtype1](implicit i: MonadControlIO[nt#apply], j: Newtype1[nt#apply, nt#oldtype1]): MonadControlIO[nt#oldtype1] = deriving[Kind.quote1[nt#oldtype1], nt](i, j.dual)

    type RunInIO[m[+_]] = MonadTransControl.RunInBase[m, IO]
}
