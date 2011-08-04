

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait MonadWriter[w, m[+_]] extends Monad[m] {
    final val asMonadWriter: MonadWriter[w, apply] = this

    // Core
    //
    def monoid: Monoid[w]
    def tell(x: w): m[Unit]
    def listen[a](x: m[a]): m[(a, w)]
    def pass[a](x: m[(a, w => w)]): m[a]

    // Extra
    //
    def listens[a, b](f: w => b)(m: m[a]): m[(a, b)] = for { (a, w) <- listen(m) } yield (a, f(w))
    def censor[a](f: w => w)(m: m[a]): m[a] = pass { for { a <- m } yield (a, f) }
}


trait MonadWriterProxy[w, m[+_]] extends MonadWriter[w, m] with MonadProxy[m] {
    override def self: MonadWriter[w, m]

    override def monoid: Monoid[w] = self.monoid
    override def tell(x: w): m[Unit] = self.tell(x)
    override def listen[a](x: m[a]): m[(a, w)] = self.listen(x)
    override def pass[a](x: m[(a, w => w)]): m[a] = self.pass(x)

    override def listens[a, b](f: w => b)(m: m[a]): m[(a, b)] = self.listens(f)(m)
    override def censor[a](f: w => w)(m: m[a]): m[a] = self.censor(f)(m)
}


object MonadWriter {
    def apply[w, m[+_]](implicit i: MonadWriter[w, m]) = i
}
