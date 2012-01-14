// Public domain

package com.github.okomok.kentest.example

import com.github.okomok.ken._

// http://jaspervdj.be/posts/2011-10-16-type-safe-events.html

class TypeSafeEventsTest extends org.scalatest.junit.JUnit3Suite {

    // *. An extensible sum type

    trait Contains[a, s] {
        type wrap = a => s
        def wrap: wrap
        type unwrap = s => Maybe[a]
        def unwrap: unwrap
    }

    sealed abstract class :+:[+a, +b]
    final case class L[a](x: a) extends :+:[a, Nothing]
    final case class R[b](x: b) extends :+:[Nothing, b]

    object :+: {
        implicit def _ofContainsL[a, b]: Contains[a, a :+: b] = new Contains[a, a :+: b] {
            override val wrap: wrap = x => L(x)
            override val unwrap: unwrap = {
                case L(x) => Just(x)
                case _ => Nothing
            }
        }

        implicit def _ofContainsR[a, b]: Contains[b, a :+: b] = new Contains[b, a :+: b]{
            override val wrap: wrap = x => R(x)
            override val unwrap: unwrap = {
                case R(x) => Just(x)
                case _ => Nothing
            }
        }

        implicit def _ofContainsContains[a, s, b](implicit _C: Contains[a, s]): Contains[a, b :+: s] = new Contains[a, b :+: s] {
            override val wrap: wrap = x => R(_C.wrap(x))
            override val unwrap: unwrap = {
                case R(x) => _C.unwrap(x)
                case _ => Nothing
            }
        }
    }

    // *. An event-aware monad

    trait MonadResponds[e, m[+_]] extends Monad[m] {
        type fire = e => m[Unit]
        def fire: fire
    }

    object MonadResponds {
        def apply[e, m <: Kind.Function1](implicit _M: MonadResponds[e, m#apply1]): MonadResponds[e, m#apply1] = _M
    }

    final case class RespondsT[e, n[+_], +a](override val old: ReaderT[e => RespondsT[e, n, Unit], n, a]) extends NewtypeOf[ReaderT[e => RespondsT[e, n, Unit], n, a]]

    object RespondsT extends RespondsTAs {
        type Next[e, n[+_]] = e => RespondsT[e, n, Unit]

        trait apply2[e, n <: Kind.Function1] extends Kind.Function1 {
            override type apply1[+a] = RespondsT[e, n#apply1, a]
        }
    }

    trait RespondsTAs0 { this: RespondsT.type =>
        implicit def _asMonadTransControl[s]: MonadTransControl[({type L[n[+_], +a] = RespondsT[s, n, a]})#L] = new MonadTransControl[({type L[n[+_], +a] = RespondsT[s, n, a]})#L] {
            private type t[n[+_], +a] = RespondsT[s, n, a]
            override def lift[n[+_], a](n: n[a])(implicit i: Monad[n]): t[n, a] = RespondsT {
                val _MT = MonadTransControl[ReaderT.apply1[Next[s, n]]]
                _MT.lift(n)
            }
            override def liftWith[n[+_], a](f: Run => n[a])(implicit i: Monad[n]): t[n, a] = error("todo")
            override def restoreT[n[+_], a](nSt: n[StT[a]])(implicit _N: Monad[n]): t[n, a] = error("todo")
        }

        implicit def _asMonad[n[+_], s](implicit _N: Monad[n]): Monad[({type L[+a] = RespondsT[s, n, a]})#L] = new Monad[({type L[+a] = RespondsT[s, n, a]})#L] {
            private type m[+a] = RespondsT[s, n, a]
            private val _R = MonadReader[ReaderT.apply2[Next[s, n], _N.type]]
            override def `return`[a](x: Lazy[a]): m[a] = RespondsT {
                _R.`return`(x)
            }
            override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = RespondsT {
                _R.op_>>=(m.old) { a => k(a).old }
            }
        }
    }

    trait RespondsTAs1 extends RespondsTAs0 { this: RespondsT.type =>
        implicit def _asMonadIO[n[+_], s](implicit _N: MonadIO[n]): MonadIO[({type L[+a] = RespondsT[s, n, a]})#L] = new MonadIO[({type L[+a] = RespondsT[s, n, a]})#L] with MonadProxy[({type L[+a] = RespondsT[s, n, a]})#L] {
            private type m[+a] = RespondsT[s, n, a]
            private val _MT = _asMonadTransControl[s]
            override val selfMonad = _asMonad[n, s]
            override def liftIO[a](io: IO[a]): m[a] = _MT.lift(_N.liftIO(io))
        }
    }

    trait RespondsTAs2 extends RespondsTAs1 { this: RespondsT.type =>
        implicit def _asMonadResponds[e, n[+_], s](implicit _C: Contains[e, s], _N: Monad[n]): MonadResponds[e, ({type L[+a] = RespondsT[s, n, a]})#L] = new MonadResponds[e, ({type L[+a] = RespondsT[s, n, a]})#L] with MonadProxy[({type L[+a] = RespondsT[s, n, a]})#L] {
            private type m[+a] = RespondsT[s, n, a]
            private val _R = MonadReader[ReaderT.apply2[Next[s, n], _N.type]]
            override val selfMonad = _asMonad[n, s]
            override val fire: fire = x => RespondsT {
                _R.op_>>=(_R.ask) { a => a(_C.wrap(x)).old }
            }
        }
    }

    trait RespondsTAs extends RespondsTAs2 { this: RespondsT.type =>
    }

    def client[m[+_], e, s](f: e => m[Unit])(s: s)(implicit _M: Monad[m], _C: Contains[e, s]): m[Unit] = {
        Maybe.maybe(_M.`return`())(f)(_C.unwrap(s))
    }

    // *. A logging client

    sealed abstract class Log
    final case class Warn(x: String) extends Log
    final case class Info(x: String) extends Log

    def logger[m[+_], s](s: s)(implicit _M: MonadIO[m], _C: Contains[Log, s]): m[Unit] = client((event: Log) =>
        _M.liftIO { IO.putStrLn {
            event match {
                case Warn(s) => "[Warn]: " ++: s
                case Info(s) => "[Info]: " ++: s
            }
        } }
    )(s)

    // *. A ping client

    sealed abstract class Ping_
    final case class Ping(x: Int) extends Ping_
    final case class Pong(x: Int) extends Ping_

    def ping[m[+_], s](s: s)(implicit _ML: MonadResponds[Log, m], _MP: MonadResponds[Ping_, m], _CP: Contains[Ping_, s]): m[Unit] = client[m, Ping_, s]({
        case Ping(x) => _MP.fire(Pong(x))
        case Pong(x) => _ML.fire(Info { "Received pong with token " ++: show(x) })
    })(s)(_ML, _CP)

    // *. Actually running it

    def combine[m[+_], e](handlers: List[e => m[Unit]])(event: e)(implicit _M: Monad[m]): m[Unit] = {
        _M.mapM_((h: e => m[Unit]) => h(event))(handlers)
    }

    type Features = Log :+: Ping_

    lazy val testClient: Features => RespondsT[Features, IO, Unit] = {
        implicit val _M = Monad[RespondsT.apply2[Features, IO.type]]
        combine[_M.apply1, Features](List(logger[_M.apply1, Features], ping[_M.apply1, Features]))
    }

    lazy val tezt: RespondsT[Features, IO, Unit] = {
        val _ML = MonadResponds[Log, RespondsT.apply2[Features, IO.type]]
        val _MP = MonadResponds[Ping_, RespondsT.apply2[Features, IO.type]]
        import _ML.`for`
        for {
            _ <- _ML.fire(Warn("Starting the engines!"))
            _ <- _MP.fire(Ping(100))
            _ <- _ML.fire(Info("Engines has been started."))
        }   _MP.fire(Ping(200))
    }

    def testTrivial {
        ignore {
            (tezt.old)(testClient).!
        }
    }
}
