

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait MonadReader[r, m[+_]] extends Monad[m] {
    final def asMonadReader: MonadReader[r, apply] = this

    // Core
    //
    def ask: m[r]
    def local[a](f: r => r)(m: m[a]): m[a]

    // Extra
    //
    def asks[a](f: r => a): m[a] = for { r <- ask } yield f(r)
}


trait MonadReaderProxy[r, m[+_]] extends MonadReader[r, m] with MonadProxy[m] {
    override def self: MonadReader[r, m]

    override def ask: m[r] = self.ask
    override def local[a](f: r => r)(m: m[a]): m[a] = self.local(f)(m)

    override def asks[a](f: r => a): m[a] = self.asks(f)
}


object MonadReader {
    def apply[r, m[+_]](implicit i: MonadReader[r, m]) = i

    implicit def ofFunction1[r]: MonadReader[r, ({type m[+a] = r => a})#m] = Function.monad[r]
}
