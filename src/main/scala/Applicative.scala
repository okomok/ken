

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Applicative[f[+_]] extends Functor[f] { outer =>
    def pure[a](x: => a): f[a]
    def op_<*>[a, b](x: f[a => b])(y: f[a]): f[b]
    def op_*>[a, b](x: f[a])(y: f[b]): f[b] = liftA2[a, b, b](const(id))(x)(y)
    def op_<*[a, b](x: f[a])(y: f[b]): f[a] = liftA2[a, b, a](const)(x)(y)

    override def fmap[a, b](x: a => b)(y: f[a]): f[b] = pure(x) <*> y

    override implicit def method[a](x: f[a]): ApplicativeMethod[f, a] = new ApplicativeMethod[f, a] {
        override def klass = outer
        override def callee = x
    }

    final def op_<**>[a, b](x: f[a])(y: f[a => b]): f[b] = liftA2[a, a => b, b](flip(op_@))(x)(y)
    final def liftA[a, b](x: a => b)(y: f[a]): f[b] = pure(x) <*> y
    final def liftA2[a, b, c](x: a => b => c)(y: f[a])(z: f[b]): f[c] = x <@> y <*> z
    final def liftA3[a, b, c, d](x: a => b => c => d)(y: f[a])(z: f[b])(w: f[c]): f[d] = x <@> y <*> z <*> w
}

trait ApplicativeMethod[f[+_], +a] extends FunctorMethod[f, a] {
    override def klass: Applicative[f]
    final def <*>[_a, b](y: f[_a])(implicit pre: f[a] <:< f[_a => b]): f[b] = klass.op_<*>(pre(callee))(y)
    final def *>[b](y: f[b]): f[b] = klass.op_*>(callee)(y)
    final def <*[_a, b](y: f[b])(implicit pre: f[a] <:< f[_a]): f[_a] = klass.op_<*(pre(callee))(y)
    final def <**>[b](y: f[a => b]): f[b] = klass.op_<**>(callee)(y)
}

trait ApplicativeProxy[f[+_]] extends Applicative[f] with FunctorProxy[f] {
    override def self: Applicative[f]
    override def pure[a](x: => a): f[a] = self.pure(x)
    override def op_<*>[a, b](x: f[a => b])(y: f[a]): f[b] = self.op_<*>(x)(y)
    override def op_*>[a, b](x: f[a])(y: f[b]): f[b] = self.op_*>(x)(y)
    override def op_<*[a, b](x: f[a])(y: f[b]): f[a] = self.op_<*(x)(y)
}


object Applicative extends ApplicativeInstance {
    def apply[f[+_]](implicit i: Applicative[f]): Applicative[f] = i

    implicit def function1[z, a](f: z => a): ApplicativeMethod[({type f[+a] = z => a})#f, a] = new ApplicativeMethod[({type f[+a] = z => a})#f, a] {
        override val klass = ofFunction1[z]
        override def callee = f
    }
}

trait ApplicativeInstance extends MonadInstance {
    implicit val ofId = Id

    implicit def ofFunction1[z]: Applicative[({type f[+a] = z => a})#f] = new Applicative[({type f[+a] = z => a})#f] {
        private[this] type f[+a] = z => a
        override def pure[a](x: => a): f[a] = const(x)
        override def op_<*>[a, b](x: f[a => b])(y: f[a]): f[b] = z => x(z)(y(z))
    }

    implicit def ofMonoid[z](implicit ma: Monoid[z]): Applicative[({type f[+a] = (z, a)})#f] = new Applicative[({type f[+a] = (z, a)})#f] {
        private[this] type f[a] = (z, a)
        override def pure[a](x: => a): f[a] = (ma.mempty, x)
        override def op_<*>[a, b](a1: f[a => b])(a2: f[a]): f[b] = (a1, a2) match {
            case ((u, f), (v, x)) => (ma.mappend(u)(v), f(x))
        }
    }
}
