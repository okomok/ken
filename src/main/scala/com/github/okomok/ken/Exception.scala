

// Copyright Shunsuke Sogame 2011.
//
// Copyright 2004, The University Court of the University of Glasgow.
// All rights reserved.
//
// Copyright (c) 2002 Simon Peyton Jones
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait Exception[e] extends Typeable[e] with Show[e] {
    final val asException: Exception[apply0] = this

    // Core
    //
    type toException = e => SomeException
    def toException: toException = e => SomeException(e)(this)

    type fromException = SomeException => Maybe[e]
    def fromException: fromException = { se => (se.rep._2).cast[e](Lazy(se.rep._1))(this) }

    // Extra
    //
    type `throw` = e => Nothing
    def `throw`: `throw` = e => Prim.raise(toException(e))

    type throwIO = e => IO[Nothing]
    def throwIO: throwIO = e => IO { Prim.raiseIO(toException(e)) }

    def `catch`[a](a: IO[a])(h: e => IO[a]): IO[a] = {
        val handler_ : SomeException => IORep[a] = e => fromException(e) match {
            case Just(e_) => IO.unIO(h(e_))
            case Nothing => throw e
        }
        IO { Prim.`catch`(IO.unIO(a))(handler_) }
    }

    def catchJust[a, b](p: e => Maybe[b])(a: IO[a])(h: b => IO[a]): IO[a] = {
        val handler_ : e => IO[a] = e => p(e) match {
            case Nothing =>`throw`(e)
            case Just(b) => h(b)
        }
        `catch`(a)(handler_)
    }

    @Annotation.flipOf("`catch`")
    final def handle[a](h: e => IO[a])(a: IO[a]): IO[a] = `catch`(a)(h)

    @Annotation.flipOf("catchJust")
    final def handleJust[a, b](p: e => Maybe[b])(h: b => IO[a])(a: IO[a]): IO[a] = catchJust(p)(a)(h)

    def mapException[e2, a](f: e => e2)(v: a)(implicit i: Exception[e2]): a = IO.unsafePerformIO(`catch`(Exception.evaluate(v))(x => i.`throw`(f(x))))

    def `try`[a](a: IO[a]): IO[Either[e, a]] = {
        import IO.{>>=, `return`}
        `catch`(a >>= (v => `return`(Right(v).of[e, a])))(e => `return`(Left(e)))
    }

    def tryJust[a, b](p: e => Maybe[b])(a: IO[a]): IO[Either[b, a]] = {
        import IO.{`for`, `return`}
        for {
            r <- `try`(a)
            * <- r match {
                case Right(v) => `return`(Right(v))
                case Left(e) => p(e) match {
                    case Nothing => `throw`(e)
                    case Just(b) => `return`(Left(b))
                }
            }
        } yield *
    }
}


trait ExceptionProxy[e] extends Exception[e] with TypeableProxy[e] with ShowProxy[e] {
    def selfException: Exception[e]
    override def selfTypeable = selfException
    override def selfShow = selfException

    override def toException: toException = selfException.toException
    override def fromException: fromException = selfException.fromException

    override def `throw`: `throw` = selfException.`throw`
    override def throwIO: throwIO = selfException.throwIO
    override def `catch`[a](a: IO[a])(h: e => IO[a]): IO[a] = selfException.`catch`(a)(h)
    override def catchJust[a, b](p: e => Maybe[b])(a: IO[a])(h: b => IO[a]): IO[a] = selfException.catchJust(p)(a)(h)
    override def mapException[e2, a](f: e => e2)(v: a)(implicit i: Exception[e2]): a = selfException.mapException(f)(v)(i)
    override def `try`[a](a: IO[a]): IO[Either[e, a]] = selfException.`try`(a)
    override def tryJust[a, b](p: e => Maybe[b])(a: IO[a]): IO[Either[b, a]] = selfException.tryJust(p)(a)
}


object Exception extends ExceptionInstance with ExceptionShortcut {
    def apply[e <: Kind.Function0](implicit i: Exception[e#apply0]): Exception[e#apply0] = i

    def deriving[nt <: Kind.Newtype0](implicit i: Exception[nt#oldtype0], j: Newtype0[nt#apply0, nt#oldtype0, _], k: Typeable[nt#apply0]): Exception[nt#apply0] = new Exception[nt#apply0] with TypeableProxy[nt#apply0] with ShowProxy[nt#apply0] {
        type e = nt#apply0
        override val selfTypeable = k
        override val selfShow = Show.deriving[nt](i, j)

        override val toException: toException = e => i.toException(j.oldOf(e))
        override val fromException: fromException = se => for { ot <- i.fromException(se) } yield j.newOf(ot)

        override val `throw`: `throw` = e => i.`throw`(j.oldOf(e))
        override val throwIO: throwIO = e => i.throwIO(j.oldOf(e))
        override def `catch`[a](a: IO[a])(h: e => IO[a]): IO[a] = i.`catch`(a)(e => h(j.newOf(e)))
        override def catchJust[a, b](p: e => Maybe[b])(a: IO[a])(h: b => IO[a]): IO[a] = i.catchJust(e => p(j.newOf(e)))(a)(h)
        override def mapException[e2, a](f: e => e2)(v: a)(implicit i2: Exception[e2]): a = i.mapException(e => f(j.newOf(e)))(v)(i2)
        override def `try`[a](a: IO[a]): IO[Either[e, a]] = for { ea <- i.`try`(a) } yield { ea match { case Left(e) => Left(j.newOf(e)); case Right(a) => Right(a) } }
        override def tryJust[a, b](p: e => Maybe[b])(a: IO[a]): IO[Either[b, a]] = i.tryJust(e => p(j.newOf(e)))(a)
    }

    def weak[nt <: Kind.Newtype0](implicit i: Exception[nt#apply0], j: Newtype0[nt#apply0, nt#oldtype0, _], k: Typeable[nt#oldtype0]): Exception[nt#oldtype0] = deriving[Kind.coNewtype0[nt]](i, j.coNewtype, k)

    @Annotation.ceremonial("no special effects")
    def evaluate[a](x: a): IO[a] = IO.`return`(x)

    @Annotation.ceremonial("no special effects")
    def mask[a](action: (IO[a] => IO[a]) => IO[a]): IO[a] = {
        val restore: IO[a] => IO[a] = act => act
        action(restore)
    }

    def onException[a, b](io: IO[a])(what: IO[b]): IO[a] = SomeException.`catch`(io) { e =>
        for {
            _ <- what
            * <- SomeException.`throw`(e): IO[a]
        } yield *
    }

    def bracket[a, b, c](before: IO[a])(after: a => IO[b])(thing: a => IO[c]): IO[c] = mask[c] { restore =>
        for {
            a <- before
            r <- onException(restore(thing(a)))(after(a))
            _ <- after(a)
        } yield r
    }

    def `finally`[a, b](a: IO[a])(sequel: IO[b]): IO[a] = mask[a] { restore =>
        for {
            r <- onException(restore(a))(sequel)
            _ <- sequel
        } yield r
    }

    def bracket_[a, b, c](before: IO[a])(after: IO[b])(thing: IO[c]): IO[c] = bracket(before)(const(after))(const(thing))

    def bracketOnError[a, b, c](before: IO[a])(after: a => IO[b])(thing: a => IO[c]): IO[c] = mask[c] { restore =>
        for {
            a <- before
            * <- onException(restore(thing(a)))(after(a))
        } yield *
    }

    def assert[a](b: Bool)(x: a): a = b match {
        case True => x
        case False => error("todo")
    }

    final case class Handler[a](rep: (e => IO[a], Exception[e]) forSome { type e })

    object Handler {
        def apply[a, e](h: e => IO[a])(implicit i: Exception[e]): Handler[a] = new Handler(h, i)
    }

    def catches[a](io: IO[a])(handlers: List[Handler[a]]): IO[a] = `catch`(io)(catchesHandler(handlers))

    def catchesHandler[a](handlers: List[Handler[a]])(e: SomeException): IO[a] = {
        val tryHandler: Handler[a] => IO[a] => IO[a] = { h => res =>
            def impl[e](x: (e => IO[a], Exception[e])): IO[a] = {
                x._2.fromException(e) match {
                    case Just(e_) => x._1(e_)
                    case Nothing => res
                }
            }
            impl(h.rep)
        }
        List.foldr(tryHandler)(SomeException.`throw`(e))(handlers)
    }

    trait AnyHandler[+a] {
        def apply[e](x: e)(implicit i: Exception[e]): IO[a]
    }

    def catchAny[a](io: IO[a])(handler: AnyHandler[a]): IO[a] = {
        val handler_ : SomeException => IORep[a] = se => {
            def impl[e](x: (e, Exception[e])): IORep[a] = {
                IO.unIO(handler(x._1)(x._2))
            }
            impl(se.rep)
        }
        IO { Prim.`catch`(IO.unIO(io))(handler_) }
    }
}


sealed trait ExceptionInstance { this: Exception.type =>
}


sealed trait ExceptionShortcut { this: Exception.type =>
    def toException[e](e: e)(implicit i: Exception[e]): SomeException = i.toException(e)
    def fromException[e](se: SomeException)(implicit i: Exception[e]): Maybe[e] = i.fromException(se)

    def `throw`[e](e: e)(implicit i: Exception[e]): Nothing = i.`throw`(e)
    def throwIO[e](e: e)(implicit i: Exception[e]): IO[Nothing] = i.throwIO(e)
    def `catch`[e, a](io: IO[a])(h: e => IO[a])(implicit i: Exception[e]): IO[a] = i.`catch`(io)(h)
    def catchJust[e, a, b](p: e => Maybe[b])(a: IO[a])(h: b => IO[a])(implicit i: Exception[e]): IO[a] = i.catchJust(p)(a)(h)
    def handle[e, a](h: e => IO[a])(a: IO[a])(implicit i: Exception[e]): IO[a] = i.handle(h)(a)
    def handleJust[e, a, b](p: e => Maybe[b])(h: b => IO[a])(a: IO[a])(implicit i: Exception[e]): IO[a] = i.handleJust(p)(h)(a)
    def mapException[e, e2, a](f: e => e2)(v: a)(implicit i: Exception[e], i2: Exception[e2]): a = i.mapException(f)(v)(i2)
    def `try`[e, a](a: IO[a], * : Type[e] = null)(implicit i: Exception[e]): IO[Either[e, a]] = i.`try`(a)
    def tryJust[e, a, b](p: e => Maybe[b])(a: IO[a])(implicit i: Exception[e]): IO[Either[b, a]] = i.tryJust(p)(a)
}
