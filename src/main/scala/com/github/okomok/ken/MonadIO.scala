

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait MonadIO[m[+_]] extends Monad[m] {
    final val asMonadIO: MonadIO[apply] = this

    // Core
    //
    def liftIO[a](io: IO[a]): m[a]

    // Monad-control
    //
    def throwIO[e](e: e)(implicit j: Exception[e]): m[Nothing] = liftIO(j.throwIO(e))
    def ioError(e: IOError): m[Nothing] = liftIO(IO.ioError(e))

    @Annotation.ceremonial("no special effects")
    final def evaluate[a](a: a): m[a] = liftIO(Exception.evaluate(a))
}


trait MonadIOProxy[m[+_]] extends MonadIO[m] with MonadProxy[m] {
    def selfMonadIO: MonadIO[m]
    override def selfMonad: Monad[m] = selfMonadIO

    override def liftIO[a](io: IO[a]): m[a] = selfMonadIO.liftIO(io)

    override def throwIO[e](e: e)(implicit j: Exception[e]): m[Nothing] = selfMonadIO.throwIO(e)(j)
    override def ioError(e: IOError): m[Nothing] = selfMonadIO.ioError(e)
}


object MonadIO extends MonadIOInstance {
    def apply[m <: Kind.Function1](implicit i: MonadIO[m#apply]): MonadIO[m#apply] = i

    def deriving[nt <: Kind.Newtype1](implicit j: Newtype1[nt#apply, nt#oldtype1], i: MonadIO[nt#oldtype1]): MonadIO[nt#apply] = new MonadIO[nt#apply] with MonadProxy[nt#apply] {
        private type m[+a] = nt#apply[a]
        override val selfMonad = Monad.deriving[nt]

        override def liftIO[a](io: IO[a]): m[a] = j.newOf { i.liftIO(io) }

        override def throwIO[e](e: e)(implicit ee: Exception[e]): m[Nothing] = j.newOf { i.throwIO(e)(ee) }
        override def ioError(e: IOError): m[Nothing] = j.newOf { i.ioError(e) }
    }

    def weak[nt <: Kind.Newtype1](implicit j: Newtype1[nt#apply, nt#oldtype1], i: MonadIO[nt#apply]): MonadIO[nt#oldtype1] = deriving[Kind.coNewtype1[nt]](j.coNewtype, i)
}


sealed trait MonadIOInstance { this: MonadIO.type =>
}
