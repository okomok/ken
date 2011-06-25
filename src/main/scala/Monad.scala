

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


import Monad._


trait Monad[m[_]] extends Applicative[m] {
    private[this] implicit val i = this

    def `return`[a](x: => a): m[a]
    def op_>>=[a, b](x: m[a])(y: a => m[b]): m[b]
    def op_>>[a, b](x: m[a])(y: => m[b]): m[b] = { lazy val _y = y; x >>= (_ => _y) }

    final override def pure[a](x: => a): m[a] = `return`(x)
    override def op_<*>[a, b](x: m[a => b])(y: m[a]): m[b] = for { _x <- x; _y <- y } yield _x(_y)
}


trait MonadPlus[m[_]] extends Monad[m] with Alternative[m] {
    def mzero[a]: m[a]
    def mplus[a](x: m[a])(y: => m[a]): m[a]

    override def empty[a]: m[a] = mzero
    override def op_<|>[a](x: m[a])(y: => m[a]): m[a] = mplus(x)(y)
}


object Monad extends MonadOp with MonadInstance


trait MonadOp extends ApplicativeOp {
    def `return`[m[_], a](x: => a)(implicit i: Monad[m]): m[a] = i.pure(x)
    def op_>>=[m[_], a, b](x: m[a])(y: a => m[b])(implicit i: Monad[m]): m[b] = i.op_>>=(x)(y)
    def op_>>[m[_], a, b](x: m[a])(y: => m[b])(implicit i: Monad[m]): m[b] = i.op_>>(x)(y)

    private[ken] class Op_>>=[m[_], a](x: m[a])(implicit i: Monad[m]) {
        def >>=[b](y: a => m[b]): m[b] = op_>>=(x)(y)
    }
    implicit def >>=[m[_], a](x: m[a])(implicit i: Monad[m]): Op_>>=[m, a] = new Op_>>=[m, a](x)

    private[ken] class Op_>>[m[_], a](x: m[a])(implicit i: Monad[m]) {
        def >>[b](y: => m[b]): m[b] = op_>>(x)(y)
    }
    implicit def >>[m[_], a, b](x: m[a])(implicit i: Monad[m]): Op_>>[m, a] = new Op_>>[m, a](x)

    private[ken] class For[m[_], a](x: m[a])(implicit i: Monad[m]) {
       def map[b](y: a => b): m[b] = op_>>=(x)(_x => `return`(y(_x)))
       def flatMap[b](y: a => m[b]): m[b] = op_>>=(x)(y)
       // def foreach(y: a => Unit): m[Unit] = op_>>=(x)(_x => `return`(y(_x)))
    }
    implicit def `for`[m[_], a](x: m[a])(implicit i: Monad[m]): For[m, a] = new For[m, a](x)

    def op_=<<[m[_], a, b](f: a => m[b])(x: m[a])(implicit i: Monad[m]): m[b] = x >>= f

    private[ken] class Op_=<<[m[_], a, b](f: a => m[b])(implicit i: Monad[m]) {
        def =<<(x: m[a]): m[b] = op_=<<(f)(x)
    }
    implicit def =<<[m[_], a, b](f: a => m[b])(implicit i: Monad[m]): Op_=<<[m, a, b] = new Op_=<<[m, a, b](f)

    def sequence[m[_], a](ms: List[m[a]])(implicit i: Monad[m]): m[List[a]] = {
        def k(m: m[a])(_m: => m[List[a]]): m[List[a]] = for { x <- m; xs <- _m } yield (x :: xs)
        List.foldr(k)(`return`(Nil))(ms)
    }

    def sequence_[m[_], a](ms: List[m[a]])(implicit i: Monad[m]): m[Unit] = {
        List.foldr(op_>>[m, a, Unit])(`return`(()))(ms)
    }

    def mapM[m[_], a, b](f: a => m[b])(as: List[a])(implicit i: Monad[m]): m[List[b]] = sequence(List.map(f)(as))
    def mapM_[m[_], a, b](f: a => m[b])(as: List[a])(implicit i: Monad[m]): m[Unit] = sequence_(List.map(f)(as))

    def filterM[m[_], a](p: a => m[Boolean])(xs: List[a])(implicit i: Monad[m]): m[List[a]] = xs match {
        case Nil => `return`(Nil)
        case x :: xs => for {
            flg <- p(x)
            ys <- filterM(p)(xs.!)
        } yield (if (flg) (x :: ys) else ys)
    }

    def forM[m[_], a, b](xs: List[a])(f: a => m[b])(implicit i: Monad[m]): m[List[b]] = mapM(f)(xs)
    def forM_[m[_], a, b](xs: List[a])(f: a => m[b])(implicit i: Monad[m]): m[Unit] = mapM_(f)(xs)

    def op_>=>[m[_], a, b, c](f: a => m[b])(g: b => m[c])(implicit i: Monad[m]): a => m[c] = { x => f(x) >>= g }
    def op_<=<[m[_], a, b, c](g: b => m[c])(f: a => m[b])(implicit i: Monad[m]): a => m[c] = op_>=>(f)(g)

    private[ken] class Op_>=>[m[_], a, b](f: a => m[b])(implicit i: Monad[m]) {
        def >=>[c](g: b => m[c]): a => m[c] = op_>=>(f)(g)
    }
    implicit def >=>[m[_], a, b](f: a => m[b])(implicit i: Monad[m]): Op_>=>[m, a, b] = new Op_>=>[m, a, b](f)

    private[ken] class Op_<=<[m[_], b, c](g: b => m[c])(implicit i: Monad[m]) {
        def <=<[a](f: a => m[b]): a => m[c] = op_<=<(g)(f)
    }
    implicit def <=<[m[_], b, c](g: b => m[c])(implicit i: Monad[m]): Op_<=<[m, b, c] = new Op_<=<[m, b, c](g)

    def forever[m[_], a](a: m[a])(implicit i: Monad[m]): m[a] = a >>= (_ => forever(a))

    def join[m[_], a](x: m[m[a]])(implicit i: Monad[m]): m[a] = x >>= id

    def mapAndUnzipM[m[_], a, b, c](f: a => m[(b, c)])(xs: List[a])(implicit i: Monad[m]): m[(List[b], List[c])] = {
        mapM(f)(xs) >>= (ys => `return`(List.unzip(ys)))
    }

    def zipWithM[m[_], a, b, c](f: a => b => m[c])(xs: List[a])(ys: List[b])(implicit i: Monad[m]): m[List[c]] = sequence(List.zipWith(f)(xs)(ys))
    def zipWithM_[m[_], a, b, c](f: a => b => m[c])(xs: List[a])(ys: List[b])(implicit i: Monad[m]): m[Unit] = sequence_(List.zipWith(f)(xs)(ys))

    def foldM[m[_], a, b](f: a => b => m[a])(a: a)(xs: List[b])(implicit i: Monad[m]): m[a] = xs match {
        case Nil => `return`(a)
        case x :: xs => f(a)(x) >>= (fax => foldM(f)(fax)(xs.!))
    }

    def foldM_[m[_], a, b](f: a => b => m[a])(a: a)(xs: List[b])(implicit i: Monad[m]): m[Unit] = {
        foldM(f)(a)(xs) >> `return`(())
    }

    def replicateM[m[_], a](n: Int)(x: m[a])(implicit i: Monad[m]): m[List[a]] = sequence(List.replicate(n)(x))
    def replicateM_[m[_], a](n: Int)(x: m[a])(implicit i: Monad[m]): m[Unit] = sequence_(List.replicate(n)(x))

    def when[m[_]](p: Boolean)(s: => m[Unit])(implicit i: Monad[m]): m[Unit] = if (p) s else `return`(())
    def unless[m[_]](p: Boolean)(s: => m[Unit])(implicit i: Monad[m]): m[Unit] = if (p) `return`(()) else s

    def liftM[m[_], a1, r](f: a1 => r)(m1: m[a1])(implicit i: Monad[m]): m[r] = for { x1 <- m1 } yield f(x1)
    def liftM2[m[_], a1, a2, r](f: a1 => a2 => r)(m1: m[a1])(m2: m[a2])(implicit i: Monad[m]): m[r] = for { x1 <- m1; x2 <- m2 } yield f(x1)(x2)
    def liftM3[m[_], a1, a2, a3, r](f: a1 => a2 => a3 => r)(m1: m[a1])(m2: m[a2])(m3: m[a3])(implicit i: Monad[m]): m[r] = for { x1 <- m1; x2 <- m2; x3 <- m3 } yield f(x1)(x2)(x3)
    def liftM4[m[_], a1, a2, a3, a4, r](f: a1 => a2 => a3 => a4 => r)(m1: m[a1])(m2: m[a2])(m3: m[a3])(m4: m[a4])(implicit i: Monad[m]): m[r] = for { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4 } yield f(x1)(x2)(x3)(x4)
    def liftM5[m[_], a1, a2, a3, a4, a5, r](f: a1 => a2 => a3 => a4 => a5 => r)(m1: m[a1])(m2: m[a2])(m3: m[a3])(m4: m[a4])(m5: m[a5])(implicit i: Monad[m]): m[r] = for { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5 } yield f(x1)(x2)(x3)(x4)(x5)

    def ap[m[_], a, b](x: m[a => b])(y: m[a])(implicit i: Monad[m]): m[b] = liftM2(id[a => b])(x)(y) // op_<*>(x)(y)

// MonadPlus
    def mzero[m[_], a](implicit i: MonadPlus[m]): m[a] = i.mzero
    def mplus[m[_], a](x: m[a])(y: => m[a])(implicit i: MonadPlus[m]): m[a] = i.mplus(x)(y)

    private[ken] class _Mplus_[m[_], a](x: m[a])(implicit i: MonadPlus[m]) {
        def _mplus_(y: => m[a]): m[a] = mplus(x)(y)
    }
    implicit def _mplus_[m[_], a](x: m[a])(implicit i: MonadPlus[m]): _Mplus_[m, a] = new _Mplus_[m, a](x)

    def guard[m[_], a](b: Boolean)(implicit i: MonadPlus[m]): m[Unit] = b match {
        case true => i.`return`(())
        case false => i.mzero
    }

    def msum[m[_], a](xs: List[m[a]])(implicit i: MonadPlus[m]): m[a] = List.foldr(i.mplus[a])(i.mzero)(xs)
}


trait MonadInstance {
}
