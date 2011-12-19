

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2010, Bas van Dijk, Anders Kaseorg
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait MonadControlIO[m[+_]] extends MonadIO[m] {
    final val asMonadControlIO: MonadControlIO[apply1] = this

    type RunInIO = MonadControlIO.RunInIO[m]

    // Core
    //
    def liftControlIO[a](f: RunInIO => IO[a]): m[a]

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

    type Handler[a] = MonadControlIO.Handler[m, a]

    object Hanlder {
        def apply[e, a](h: e => m[a])(implicit i: Exception[e]): Handler[a] = new Handler(h, i)
        def unapply[a](x: Handler[a]): Option[(e => m[a], Exception[e]) forSome { type e }] = Some(x.rep) // doesn't work for now; SI-5022
    }

    def catches[a](a: m[a])(handlers: List[Handler[a]]): m[a] = controlIO { runInIO =>
        Exception.catches(runInIO(a)) {
            for {
                h <- handlers
            } yield {
                def impl[e](rep: (e => m[a], Exception[e])): Exception.Handler[m[a]] = {
                    Exception.Handler((e: e) => runInIO { rep._1(e) })(rep._2)
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

    override def controlIO[a](f: RunInIO => IO[m[a]]): m[a] = join(liftControlIO(f))
    override def liftIOOp[a, b, c](f: (a => IO[m[b]]) => IO[m[c]]): (a => m[b]) => m[c] = selfMonadControlIO.liftIOOp(f)
    override def liftIOOp_[a, b](f: IO[m[a]] => IO[m[b]]): m[a] => m[b] = selfMonadControlIO.liftIOOp_(f)

    override def `catch`[e, a](a: m[a])(handler: e => m[a])(implicit j: Exception[e]): m[a] = selfMonadControlIO.`catch`(a)(handler)(j)
    override def catches[a](a: m[a])(handlers: List[Handler[a]]): m[a] = selfMonadControlIO.catches(a)(handlers)
    override def catchJust[e, a, b](p: e => Maybe[b])(a: m[a])(handler: b => m[a])(implicit j: Exception[e]): m[a] = selfMonadControlIO.catchJust(p)(a)(handler)(j)
    override def handle[e, a](handler: e => m[a])(a: m[a])(implicit j: Exception[e]): m[a] = selfMonadControlIO.handle(handler)(a)(j)
    override def handleJust[e, a, b](p: e => Maybe[b])(handler: b => m[a])(a: m[a])(implicit j: Exception[e]): m[a] = selfMonadControlIO.handleJust(p)(handler)(a)(j)
    override def `try`[e, a](a: m[a])(implicit j: Exception[e]): m[Either[e, a]] = selfMonadControlIO.`try`(a)(j)
    override def tryJust[e, a, b](p: e => Maybe[b])(a: m[a])(implicit j: Exception[e]): m[Either[b, a]] = selfMonadControlIO.tryJust(p)(a)(j)
    override def bracket[a, b, c](before: m[a])(after: a => m[b])(thing: a => m[c]): m[c] = selfMonadControlIO.bracket(before)(after)(thing)
    override def bracket_[a, b, c](before: m[a])(after: m[b])(thing: m[c]): m[c] = selfMonadControlIO.bracket_(before)(after)(thing)
    override def bracketOnError[a, b, c](before: m[a])(after: a => m[b])(thing: a => m[c]): m[c] = selfMonadControlIO.bracketOnError(before)(after)(thing)
    override def `finally`[a, b](a: m[a])(sequel: m[b]): m[a] = selfMonadControlIO.`finally`(a)(sequel)
    override def onException[a, b](m: m[a])(what: m[b]): m[a] = selfMonadControlIO.onException(m)(what)
}


object MonadControlIO {
    def apply[m <: Kind.Function1](implicit i: MonadControlIO[m#apply1]): MonadControlIO[m#apply1] = i

    def deriving[nt <: Kind.Newtype1](implicit j: Newtype1[nt#apply1, nt#oldtype1], i: MonadControlIO[nt#oldtype1]): MonadControlIO[nt#apply1] = new MonadControlIO[nt#apply1] with MonadIOProxy[nt#apply1] {
        private type m[+a] = nt#apply1[a]
        override val selfMonadIO = MonadIO.deriving[nt]

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

    def weak[nt <: Kind.Newtype1](implicit j: Newtype1[nt#apply1, nt#oldtype1], i: MonadControlIO[nt#apply1]): MonadControlIO[nt#oldtype1] = deriving[Kind.coNewtype1[nt]](j.coNewtype, i)

    type RunInIO[m[+_]] = MonadTrans.RunInBase[m, IO]

    case class Handler[m[+_], a](rep: (e => m[a], Exception[e]) forSome { type e }) {
        def apply[r](f: Function[(e => m[a], Exception[e]) forSome { type e }, r]): r = f(rep)
    }

    object Handler {
        def apply[e, m[+_], a](h: e => m[a])(implicit i: Exception[e]): Handler[m, a] = new Handler(h, i)
    }
}
