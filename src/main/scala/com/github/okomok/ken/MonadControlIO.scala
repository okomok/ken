

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
        override val selfMonadIO = MonadIO.deriving[nt, ot](i, j)
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
