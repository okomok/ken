

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


import Applicative._


trait Applicative[f[_]] extends Functor[f] {
    private[this] implicit val i = this

    def pure[a](x: => a): f[a]
    def op_<*>[a, b](x: f[a => b])(y: f[a]): f[b]
    def op_*>[a, b](x: f[a])(y: f[b]): f[b] = liftA2[f, a, b, b](const(id))(x)(y)
    def op_<*[a, b](x: f[a])(y: f[b]): f[a] = liftA2[f, a, b, a](const)(x)(y)

    override def fmap[a, b](x: a => b)(y: f[a]): f[b] = pure(x) <*> y
}


trait Alternative[f[_]] extends Applicative[f] {
    private[this] implicit val i = this

    def empty[a]: f[a]
    def op_<|>[a](x: f[a])(y: => f[a]): f[a]

    def some[a](v: f[a]): f[List[a]] = {
        def many_v: f[List[a]] = some_v <|> pure(Nil)
        def some_v: f[List[a]] = _cons[a] <@> v <*> many_v
        some_v
    }

    def many[a](v: f[a]): f[List[a]] = {
        def many_v: f[List[a]] = some_v <|> pure(Nil)
        def some_v: f[List[a]] = _cons[a] <@> v <*> many_v
        many_v
    }

    private[this] def _cons[a]: a => List[a] => List[a] = x => xs => x :: xs
}


object Applicative extends ApplicativeOp with ApplicativeInstance


trait ApplicativeOp extends FunctorOp {
    def pure[f[_], a](x: => a)(implicit i: Applicative[f]): f[a] = i.pure(x)
    def op_<*>[f[_], a, b](x: f[a => b])(y: f[a])(implicit i: Applicative[f]): f[b] = i.op_<*>(x)(y)

    def op_*>[f[_], a, b](x: f[a])(y: f[b])(implicit i: Applicative[f]): f[b] = i.op_*>(x)(y)
    def op_<*[f[_], a, b](x: f[a])(y: f[b])(implicit i: Applicative[f]): f[a] = i.op_<*(x)(y)

    def op_<@>[f[_], a, b](x: a => b)(y: f[a])(implicit i: Functor[f]): f[b] = i.fmap(x)(y)

    private[ken] class Op_<@>[f[_], a, b](x: a => b)(implicit i: Functor[f]) {
        def <@>(y: f[a]): f[b] = op_<@>(x)(y)
    }
    implicit def <@>[f[_], a, b](x: a => b)(implicit i: Functor[f]): Op_<@>[f, a, b] = new Op_<@>[f, a, b](x)

    def op_<@[f[_], a, b](x: => a)(y: f[b])(implicit i: Functor[f]): f[a] = i.fmap[b, a](_ => x)(y)

    private[ken] class Op_<@[f[_], a](x: => a)(implicit i: Functor[f]) {
        def <@[b](y: f[b]): f[a] = op_<@(x)(y)
    }
    implicit def <@[f[_], a](x: => a)(implicit i: Functor[f]): Op_<@[f, a] = new Op_<@[f, a](x)

    private[ken] class Op_<*>[f[_], a, b](x: f[a => b])(implicit i: Applicative[f]) {
        def <*>(y: f[a]): f[b] = op_<*>(x)(y)
    }
    implicit def <*>[f[_], a, b](x: f[a => b])(implicit i: Applicative[f]): Op_<*>[f, a, b] = new Op_<*>[f, a, b](x)

    private[ken] class Op_*>[f[_], a](x: f[a])(implicit i: Applicative[f]) {
        def *>[b](y: f[b]): f[b] = op_*>(x)(y)
    }
    implicit def *>[f[_], a, b](x: f[a])(implicit i: Applicative[f]): Op_*>[f, a] = new Op_*>[f, a](x)

    private[ken] class Op_<*[f[_], a](x: f[a])(implicit i: Applicative[f]) {
        def <*[b](y: f[b]): f[a] = op_<*(x)(y)
    }
    implicit def <*[f[_], a](x: f[a])(implicit i: Applicative[f]): Op_<*[f, a] = new Op_<*[f, a](x)

    private[ken] class Op_<**>[f[_], a](x: f[a])(implicit i: Applicative[f]) {
        def <**>[b](y: f[a => b]): f[b] = op_<**>(x)(y)
    }
    implicit def <**>[f[_], a](x: f[a])(implicit i: Applicative[f]): Op_<**>[f, a] = new Op_<**>[f, a](x)

    def op_<**>[f[_], a, b](x: f[a])(y: f[a => b])(implicit i: Applicative[f]): f[b] = liftA2[f, a, a => b, b](flip(`@`))(x)(y)
    def liftA[f[_], a, b](x: a => b)(y: f[a])(implicit i: Applicative[f]): f[b] = pure(x)(i) <*> y
    def liftA2[f[_], a, b, c](x: a => b => c)(y: f[a])(z: f[b])(implicit i: Applicative[f]): f[c] = x <@> y <*> z
    def liftA3[f[_], a, b, c, d](x: a => b => c => d)(y: f[a])(z: f[b])(w: f[c])(implicit i: Applicative[f]): f[d] = x <@> y <*> z <*> w

// Alternative
    def empty[f[_], a](implicit i: Alternative[f]): f[a] = i.empty[a]
    def op_<|>[f[_], a](x: f[a])(y: => f[a])(implicit i: Alternative[f]): f[a] = i.op_<|>(x)(y)

    def some[f[_], a](v: f[a])(implicit i: Alternative[f]): f[List[a]] = i.some(v)
    def many[f[_], a](v: f[a])(implicit i: Alternative[f]): f[List[a]] = i.many(v)

    private[ken] class Op_<|>[f[_], a](x: f[a])(implicit i: Alternative[f]) {
        def <|>(y: => f[a]): f[a] = op_<|>(x)(y)
    }
    implicit def <|>[f[_], a](x: f[a])(implicit i: Alternative[f]): Op_<|>[f, a] = new Op_<|>[f, a](x)

    def optional[f[_], a](x: f[a])(implicit i: Alternative[f]): f[Maybe[a]] = (Just(_: a).of[a]) <@> x <|> pure(Nothing.of[a])(i)
}


trait ApplicativeInstance extends MonadInstance {
    implicit val ofId = Id

    implicit def ofFunction1[A]: Applicative[({type f[a] = A => a})#f] = new Applicative[({type f[a] = A => a})#f] {
        private[this] type f[a] = A => a
        override def pure[a](x: => a): f[a] = const(x)
        override def op_<*>[a, b](x: f[a => b])(y: f[a]): f[b] = z => x(z)(y(z))
    }

    implicit def ofMonoid[a](implicit ma: Monoid[a]): Applicative[({type f[x] = (a, x)})#f] = new Applicative[({type f[x] = (a, x)})#f] {
        private[this] type f[x] = (a, x)
        override def pure[a](x: => a): f[a] = (ma.mempty, x)
        override def op_<*>[a, b](a1: f[a => b])(a2: f[a]): f[b] = (a1, a2) match {
            case ((u, f), (v, x)) => (ma.mappend(u)(v), f(x))
        }
    }
}
