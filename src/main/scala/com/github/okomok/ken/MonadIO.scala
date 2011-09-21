

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


object MonadIO {
    def apply[m <: Kind.Function1](implicit i: MonadIO[m#apply]): MonadIO[m#apply] = i

    def deriving[nt <: Kind.Function1, ot <: Kind.Function1](implicit i: MonadIO[ot#apply], j: Newtype1[nt#apply, ot#apply]): MonadIO[nt#apply] = new MonadIO[nt#apply] with MonadProxy[nt#apply] {
        private type m[+a] = nt#apply[a]
        override val selfMonad = Monad.deriving[nt, ot](i, j)
        override def liftIO[a](io: IO[a]): m[a] = j.newOf { i.liftIO(io) }
    }

    def weak[nt <: Kind.Newtype1](implicit i: MonadIO[nt#apply], j: Newtype1[nt#apply, nt#oldtype1]): MonadIO[nt#oldtype1] = deriving[Kind.quote1[nt#oldtype1], nt](i, j.dual)
}
