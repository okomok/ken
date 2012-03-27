

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait MonadIO[m[+_]] extends Monad[m] {
    final val asMonadIO: MonadIO[apply1] = this

    // Core
    //
    def liftIO[a](io: IO[a]): m[a]

    // Throwing exceptions (from monad-control)
    //
    def throwIO[e](e: e)(implicit _E: Exception[e]): m[Nothing] = liftIO(_E.throwIO(e))

    type ioError = IOError => m[Nothing]
    def ioError: ioError = e => liftIO(IO.ioError(e))

    // @ceremonial("no special effects")
    final def evaluate[a](a: a): m[a] = liftIO(Exception.evaluate(a))
}


trait MonadIOProxy[m[+_]] extends MonadIO[m] with MonadProxy[m] {
    type selfMonadIO = MonadIO[m]
    def selfMonadIO: selfMonadIO
    override def selfMonad: selfMonad = selfMonadIO

    override def liftIO[a](io: IO[a]): m[a] = selfMonadIO.liftIO(io)

    override def throwIO[e](e: e)(implicit _E: Exception[e]): m[Nothing] = selfMonadIO.throwIO(e)(_E)
    override def ioError: ioError = selfMonadIO.ioError
}


object MonadIO extends MonadIOInstance {
    def apply[m <: Kind.Function1](implicit i: MonadIO[m#apply1]): MonadIO[m#apply1] = i

    def deriving[nt <: Kind.Newtype1](implicit j: Newtype1[nt#apply1, nt#oldtype1], i: MonadIO[nt#oldtype1]): MonadIO[nt#apply1] = new MonadIO[nt#apply1] with MonadProxy[nt#apply1] {
        private type m[+a] = nt#apply1[a]
        override val selfMonad: selfMonad = Monad.deriving[nt]

        override def liftIO[a](io: IO[a]): m[a] = j.newOf { i.liftIO(io) }

        override def throwIO[e](e: e)(implicit _E: Exception[e]): m[Nothing] = j.newOf { i.throwIO(e)(_E) }
        override def ioError: ioError = e => j.newOf { i.ioError(e) }
    }

    def weak[nt <: Kind.Newtype1](implicit j: Newtype1[nt#apply1, nt#oldtype1], i: MonadIO[nt#apply1]): MonadIO[nt#oldtype1] = deriving[Kind.coNewtype1[nt]](j.coNewtype, i)
}


sealed trait MonadIOInstance { this: MonadIO.type =>
}
