

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait MonadIO[m[+_]] extends Monad[m] {
    final val asMonadIO: MonadIO[apply] = this

    type RunInIO = MonadIO.RunInIO[m]

    // Core
    //
    def liftIO[a](io: IO[a]): m[a]
    def liftControlIO[a](f: RunInIO => IO[a]): m[a] = error("todo")

    // Extra
    //
    def controlIO[a](f: RunInIO => IO[m[a]]): m[a] = join(liftControlIO(f))

    def liftIOOp[a, b, c](f: (a => IO[m[b]]) => IO[m[c]]): (a => m[b]) => m[c] = g => controlIO { runInIO => f { a => runInIO(g(a)) } }
    def liftIOOp_[a, b](f: IO[m[a]] => IO[m[b]]): m[a] => m[b] = m => controlIO { runInIO => f { runInIO(m) } }
}


trait MonadIOProxy[m[+_]] extends MonadIO[m] with MonadProxy[m] {
    def selfMonadIO: MonadIO[m]
    override def selfMonad: Monad[m] = selfMonadIO

    override def liftIO[a](io: IO[a]): m[a] = selfMonadIO.liftIO(io)
    override def liftControlIO[a](f: RunInIO => IO[a]): m[a] = selfMonadIO.liftControlIO(f)

    override def controlIO[a](f: RunInIO => IO[m[a]]): m[a] = selfMonadIO.controlIO(f)

    override def liftIOOp[a, b, c](f: (a => IO[m[b]]) => IO[m[c]]): (a => m[b]) => m[c] = selfMonadIO.liftIOOp(f)
    override def liftIOOp_[a, b](f: IO[m[a]] => IO[m[b]]): m[a] => m[b] = selfMonadIO.liftIOOp_(f)
}


object MonadIO {
    def apply[m <: Kind.Function1](implicit i: MonadIO[m#apply]): MonadIO[m#apply] = i

    def deriving[nt <: Kind.Function1, ot <: Kind.Function1](implicit i: MonadIO[ot#apply], j: Newtype1[nt#apply, ot#apply]): MonadIO[nt#apply] = new MonadIO[nt#apply] with MonadProxy[nt#apply] {
        private type m[+a] = nt#apply[a]
        override val selfMonad = Monad.deriving[nt, ot](i, j)
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

    def weak[nt <: Kind.Newtype1](implicit i: MonadIO[nt#apply], j: Newtype1[nt#apply, nt#oldtype1]): MonadIO[nt#oldtype1] = deriving[Kind.quote1[nt#oldtype1], nt](i, j.dual)

    type RunInIO[m[+_]] = MonadTransControl.RunInBase[m, IO]
}
