

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2010, Bas van Dijk, Anders Kaseorg
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


// This is not a typeclass.


trait MonadIOControl[m[+_]] extends MonadBaseControl[IO, m] with MonadIO[m] {
    final val asMonadIOControl: MonadIOControl[apply1] = this

    // Overrides
    //
    // MonadBaseControl
    override def baseMonad: baseMonad = IO
    // MonadIO
    override def liftIO[a](b: IO[a]): m[a] = liftBase(b)

    // Synonyms
    //
    type RunInIO = RunInBase
    final def liftIOWith[a](f: RunInIO => IO[a]): m[a] = liftBaseWith(f)
    final def liftIOOp[a, q, c](f: (a => IO[StM[q]]) => IO[StM[c]]): (a => m[q]) => m[c] = liftBaseOp(f)
    final def liftIOOp_[a, q](f: IO[StM[a]] => IO[StM[q]]): m[a] => m[q] = liftBaseOp_(f)
    final def liftIODiscard[a](f: IO[Unit] => IO[a]): m[Unit] => m[a] = liftBaseDiscard(f)

    // Catching exceptions
    //
    def `catch`[e, a](a: m[a])(handler: e => m[a])(implicit _E: Exception[e]): m[a] = control { runInIO =>
        _E.`catch`(runInIO(a))(e => runInIO { handler(e) })
    }

    type Handler[a] = MonadIOControl.Handler[m, a]

    object Hanlder {
        def apply[e, a](h: e => m[a])(implicit i: Exception[e]): Handler[a] = new Handler(h, i)
        def unapply[a](x: Handler[a]): Option[(e => m[a], Exception[e]) forSome { type e }] = Some(x.rep) // doesn't work for now; SI-5022
    }

    def catches[a](a: m[a])(handlers: List[Handler[a]]): m[a] = control { runInIO =>
        Exception.catches(runInIO(a)) {
            for {
                h <- handlers
            } yield {
                def impl[e](rep: (e => m[a], Exception[e])): Exception.Handler[StM[a]] = {
                    Exception.Handler((e: e) => runInIO { rep._1(e) })(rep._2)
                }
                impl(h.rep)
            }
        }
    }

    def catchJust[e, a, b](p: e => Maybe[b])(a: m[a])(handler: b => m[a])(implicit _E: Exception[e]): m[a] = control { runInIO =>
        _E.catchJust(p)(runInIO(a))(e => runInIO(handler(e)))
    }

    // The handle functions
    //
    def handle[e, a](handler: e => m[a])(a: m[a])(implicit _E: Exception[e]): m[a] = control { runInIO =>
        _E.handle(e => runInIO(handler(e)))(runInIO(a))
    }

    def handleJust[e, a, b](p: e => Maybe[b])(handler: b => m[a])(a: m[a])(implicit _E: Exception[e]): m[a] = control { runInIO =>
        _E.handleJust(p)(e => runInIO(handler(e)))(runInIO(a))
    }

    // The try functions
    //
    def sequenceEither[e, a](x: Either[e, StM[a]]): m[Either[e, a]] = {
        Either.either((e: e) => `return`(Left(e).of[e, a]))((st: StM[a]) => liftM((a: a) => Right(a).of[e, a])(restoreM(st)))(x)
    }

    def `try`[e, a](m: m[a])(implicit _E: Exception[e]): m[Either[e, a]] = {
        liftIOWith { runInIO =>
            _E.`try`(runInIO(m))
        } >>= { (x: Either[e, StM[a]]) =>
            sequenceEither(x)
        }
    }

    def tryJust[e, a, b](p: e => Maybe[b])(m: m[a])(implicit _E: Exception[e]): m[Either[b, a]] = {
        liftIOWith { runInIO =>
            _E.tryJust(p)(runInIO(m))
        } >>= { (x: Either[b, StM[a]]) =>
            sequenceEither(x)
        }
    }

    // Brackets
    //
    def bracket[a, b, c](before: m[a])(after: a => m[b])(thing: a => m[c]): m[c] = control { runInIO =>
        Exception.bracket(runInIO(before))((st: StM[a]) => runInIO { restoreM(st) >>= after })((st: StM[a]) => runInIO { restoreM(st) >>= thing })
    }

    def bracket_[a, b, c](before: m[a])(after: m[b])(thing: m[c]): m[c] = control { runInIO =>
        Exception.bracket_(runInIO(before))(runInIO(after))(runInIO(thing))
    }

    def bracketOnError[a, b, c](before: m[a])(after: a => m[b])(thing: a => m[c]): m[c] = control { runInIO =>
        Exception.bracketOnError(runInIO(before))((st: StM[a]) => runInIO { restoreM(st) >>= after })((st: StM[a]) => runInIO { restoreM(st) >>= thing })
    }

    // Utilities
    //
    def `finally`[a, b](a: m[a])(sequel: m[b]): m[a] = control { runInIO =>
        Exception.`finally`(runInIO(a))(runInIO(sequel))
    }

    def onException[a, b](m: m[a])(what: m[b]): m[a] = control { runInIO =>
        Exception.onException(runInIO(m))(runInIO(what))
    }
}


trait MonadIOControlProxy[m[+_]] extends MonadIOControl[m] with MonadBaseControlProxy[IO, m] with MonadIOProxy[m] {
    type selfMonadIOControl = MonadIOControl[m]
    def selfMonadIOControl: selfMonadIOControl
    override val selfMonadBaseControl: selfMonadBaseControl = selfMonadIOControl
    override def selfMonadIO: selfMonadIO = selfMonadIOControl

    override def `catch`[e, a](a: m[a])(handler: e => m[a])(implicit _E: Exception[e]): m[a] = selfMonadIOControl.`catch`(a)(handler)(_E)
    override def catches[a](a: m[a])(handlers: List[Handler[a]]): m[a] = selfMonadIOControl.catches(a)(handlers)
    override def catchJust[e, a, b](p: e => Maybe[b])(m: m[a])(handler: b => m[a])(implicit _E: Exception[e]): m[a] = selfMonadIOControl.catchJust(p)(m)(handler)(_E)
    override def handle[e, a](handler: e => m[a])(a: m[a])(implicit _E: Exception[e]): m[a] = selfMonadIOControl.handle(handler)(a)(_E)
    override def handleJust[e, a, b](p: e => Maybe[b])(handler: b => m[a])(a: m[a])(implicit _E: Exception[e]): m[a] = selfMonadIOControl.handleJust(p)(handler)(a)(_E)
    override def `try`[e, a](a: m[a])(implicit _E: Exception[e]): m[Either[e, a]] = selfMonadIOControl.`try`(a)(_E)
    override def tryJust[e, a, b](p: e => Maybe[b])(a: m[a])(implicit _E: Exception[e]): m[Either[b, a]] = selfMonadIOControl.tryJust(p)(a)(_E)
    override def bracket[a, b, c](before: m[a])(after: a => m[b])(thing: a => m[c]): m[c] = selfMonadIOControl.bracket(before)(after)(thing)
    override def bracket_[a, b, c](before: m[a])(after: m[b])(thing: m[c]): m[c] = selfMonadIOControl.bracket_(before)(after)(thing)
    override def bracketOnError[a, b, c](before: m[a])(after: a => m[b])(thing: a => m[c]): m[c] = selfMonadIOControl.bracketOnError(before)(after)(thing)
    override def `finally`[a, b](a: m[a])(sequel: m[b]): m[a] = selfMonadIOControl.`finally`(a)(sequel)
    override def onException[a, b](m: m[a])(what: m[b]): m[a] = selfMonadIOControl.onException(m)(what)
}


object MonadIOControl {
    def apply[m <: Kind.Function1](implicit _B: MonadBaseControl[IO, m#apply1]): MonadIOControl[m#apply1] = new MonadIOControl[m#apply] with MonadBaseControlProxy[IO, m#apply] {
        override val selfMonadBaseControl: selfMonadBaseControl = _B
    }

    case class Handler[m[+_], a](rep: (e => m[a], Exception[e]) forSome { type e }) {
        def apply[r](f: Function[(e => m[a], Exception[e]) forSome { type e }, r]): r = f(rep)
    }

    object Handler {
        def apply[e, m[+_], a](h: e => m[a])(implicit i: Exception[e]): Handler[m, a] = new Handler(h, i)
    }
}
