

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait MonadIO[m[+_]] extends Monad[m] {
    final val asMonadIO: MonadIO[apply] = this

    // Core
    //
    def liftIO[a](io: IO[a]): m[a]
}


trait MonadIOProxy[m[+_]] extends MonadIO[m] with MonadProxy[m] {
    override def self: MonadIO[m]

    override def liftIO[a](io: IO[a]): m[a] = self.liftIO(io)
}


object MonadIO {
    def apply[m <: Kind.Function1](implicit i: MonadIO[m#apply]): MonadIO[m#apply] = i
}
