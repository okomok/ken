

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


import Prelude._
import List.{Nil, ::}


trait Monad[m[_]] extends Applicative[m] {
    final def `return`[a](x: => a): m[a] = pure(x)
    def op_>>=[a, b](x: m[a])(y: a => m[b]): m[b]
    def op_>>[a, b](x: m[a])(y: m[b]): m[b] = x >>= (_ => y)

    private[ken] class Op_>>=[a](x: m[a]) {
        def >>=[b](y: a => m[b]): m[b] = op_>>=(x)(y)
    }
    implicit def >>=[a](x: m[a]): Op_>>=[a] = new Op_>>=(x)

    private[ken] class Op_>>[a](x: m[a]) {
        def >>[b](y: m[b]): m[b] = op_>>(x)(y)
    }
    implicit def >>[a, b](x: m[a]): Op_>>[a] = new Op_>>(x)

    // will work only in generic context.
    private[ken] class ForExpr[a](x: m[a]) {
       def map[b](y: a => b): m[b] = op_>>=(x)(_x => pure(y(_x)))
       def flatMap[b](y: a => m[b]): m[b] = op_>>=(x)(y)
    }
    implicit def forExpr[a](x: m[a]): ForExpr[a] = new ForExpr(x)

    override def op_<*>[a, b](x: m[a => b])(y: m[a]): m[b] = {
        // x >>= (_x => y >>= (_y => pure(_x(_y))))
        for { _x <- x; _y <- y } yield _x(_y)
    }

    final def op_=<<[a, b](f: a => m[b])(x: m[a]): m[b] = x >>= f

    private[ken] class Op_=<<[a, b](f: a => m[b]) {
        def =<<(x: m[a]): m[b] = op_=<<(f)(x)
    }
    implicit def =<<[a, b](f: a => m[b]): Op_=<<[a, b] = new Op_=<<(f)

    final def sequence[a](ms: List[m[a]]): m[List[a]] = {
        def k(m: m[a])(_m: Lazy[m[List[a]]]): m[List[a]] = for { x <- m; xs <- _m() } yield (x :: xs)
        foldr(k)(`return`(Nil))(ms)
    }

    final def sequence_[a](ms: List[m[a]]): m[Unit] = {
        foldr[m[a], m[Unit]](Lazy.r(op_>>))(`return`(()))(ms)
    }

    final def mapM[a, b](f: a => m[b])(as: List[a]): m[List[b]] = sequence(map(f)(as))

    final def mapM_[a, b](f: a => m[b])(as: List[a]): m[Unit] = sequence_(map(f)(as))

    final def filterM[a](p: a => m[Boolean])(xs: List[a]): m[List[a]] = xs match {
        case Nil => `return`(Nil)
        case x :: xs => for {
            flg <- p(x)
            ys <- filterM(p)(xs())
        } yield (if (flg) (x :: ys) else ys)
    }

    final def forever[a, b](a: m[a]): m[b] = a >> forever(a)

    final def ap[a, b](x: m[a => b])(y: m[a]): m[b] = op_<*>(x)(y)
}
