

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Monad[m[+_]] extends Applicative[m] {
// Overridables
    def `return`[a](x: a): m[a]
    def op_>>=[a, b](x: m[a])(y: a => m[b]): m[b]
    def op_>>[b](x: m[_])(y: => m[b]): m[b] = { lazy val _y = y; x >>= (_ => _y) }

// Overrides
    override def pure[a](x: => a): m[a] = `return`(x)
    override def op_<*>[a, b](x: m[a => b])(y: m[a]): m[b] = for { _x <- x; _y <- y } yield _x(_y)

// Utilities
    final def op_=<<[a, b](f: a => m[b])(x: m[a]): m[b] = x >>= f

    final def sequence[a](ms: List[m[a]]): m[List[a]] = {
        def k(m: m[a])(_m: => m[List[a]]): m[List[a]] = for { x <- m; xs <- _m } yield (x :: xs)
        List.foldr(k)(`return`(Nil))(ms)
    }

    final def sequence_[a](ms: List[m[a]]): m[Unit] = {
        List.foldr(op_>>[Unit])(`return`(()))(ms)
    }

    final def mapM[a, b](f: a => m[b])(as: List[a]): m[List[b]] = sequence(List.map(f)(as))
    final def mapM_[a, b](f: a => m[b])(as: List[a]): m[Unit] = sequence_(List.map(f)(as))

    final def filterM[a](p: a => m[Bool])(xs: List[a]): m[List[a]] = xs match {
        case Nil => `return`(Nil)
        case x :: xs => for {
            flg <- p(x)
            ys <- filterM(p)(xs.!)
        } yield (if (flg) (x :: ys) else ys)
    }

    final def forM[a, b](xs: List[a])(f: a => m[b]): m[List[b]] = mapM(f)(xs)
    final def forM_[a, b](xs: List[a])(f: a => m[b]): m[Unit] = mapM_(f)(xs)

    final def op_>=>[a, b, c](f: a => m[b])(g: b => m[c]): a => m[c] = { x => f(x) >>= g }
    final def op_<=<[a, b, c](g: b => m[c])(f: a => m[b]): a => m[c] = op_>=>(f)(g)

    final private[ken] class Op_>=>[a, b](f: a => m[b]) {
        def >=>[c](g: b => m[c]): a => m[c] = op_>=>(f)(g)
    }
    final implicit def >=>[a, b](f: a => m[b]): Op_>=>[a, b] = new Op_>=>[a, b](f)

    final private[ken] class Op_<=<[b, c](g: b => m[c]) {
        def <=<[a](f: a => m[b]): a => m[c] = op_<=<(g)(f)
    }
    final implicit def <=<[b, c](g: b => m[c]): Op_<=<[b, c] = new Op_<=<[b, c](g)

    final def forever[a](a: m[a]): m[a] = a >>= (_ => forever(a))

    final def join[a](x: m[m[a]]): m[a] = x >>= id

    final def mapAndUnzipM[a, b, c](f: a => m[(b, c)])(xs: List[a]): m[(List[b], List[c])] = {
        mapM(f)(xs) >>= (ys => `return`(List.unzip(ys)))
    }

    final def zipWithM[a, b, c](f: a => b => m[c])(xs: List[a])(ys: List[b]): m[List[c]] = sequence(List.zipWith(f)(xs)(ys))
    final def zipWithM_[a, b, c](f: a => b => m[c])(xs: List[a])(ys: List[b]): m[Unit] = sequence_(List.zipWith(f)(xs)(ys))

    final def foldM[a, b](f: a => b => m[a])(a: a)(xs: List[b]): m[a] = xs match {
        case Nil => `return`(a)
        case x :: xs => f(a)(x) >>= (fax => foldM(f)(fax)(xs.!))
    }

    final def foldM_[a, b](f: a => b => m[a])(a: a)(xs: List[b]): m[Unit] = {
        foldM(f)(a)(xs) >> `return`(())
    }

    final def replicateM[a](n: Int)(x: m[a]): m[List[a]] = sequence(List.replicate(n)(x))
    final def replicateM_[a](n: Int)(x: m[a]): m[Unit] = sequence_(List.replicate(n)(x))

    final def when(p: Bool)(s: => m[Unit]): m[Unit] = if (p) s else `return`()
    final def unless(p: Bool)(s: => m[Unit]): m[Unit] = if (p) `return`() else s

    final def liftM[a1, r](f: a1 => r)(m1: m[a1]): m[r] = for { x1 <- m1 } yield f(x1)
    final def liftM2[a1, a2, r](f: a1 => a2 => r)(m1: m[a1])(m2: m[a2]): m[r] = for { x1 <- m1; x2 <- m2 } yield f(x1)(x2)
    final def liftM3[a1, a2, a3, r](f: a1 => a2 => a3 => r)(m1: m[a1])(m2: m[a2])(m3: m[a3]): m[r] = for { x1 <- m1; x2 <- m2; x3 <- m3 } yield f(x1)(x2)(x3)
    final def liftM4[a1, a2, a3, a4, r](f: a1 => a2 => a3 => a4 => r)(m1: m[a1])(m2: m[a2])(m3: m[a3])(m4: m[a4]): m[r] = for { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4 } yield f(x1)(x2)(x3)(x4)
    final def liftM5[a1, a2, a3, a4, a5, r](f: a1 => a2 => a3 => a4 => a5 => r)(m1: m[a1])(m2: m[a2])(m3: m[a3])(m4: m[a4])(m5: m[a5]): m[r] = for { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5 } yield f(x1)(x2)(x3)(x4)(x5)

    final def ap[a, b](x: m[a => b])(y: m[a]): m[b] = liftM2(id[a => b])(x)(y) // op_<*>(x)(y)

    final val monadT: MonadT[m] = new MonadT[m]()(this)

// Infix Operators
    sealed class Infix_>>=[a](x: m[a]) {
        def >>=[b](y: a => m[b]): m[b] = op_>>=(x)(y)
    }
    final implicit def >>=[a](x: m[a]): Infix_>>=[a] = new Infix_>>=(x)

    sealed class Infix_>>(x: m[_]) {
        def >>[b](y: m[b]): m[b] = op_>>(x)(y)
    }
    final implicit def >>(x: m[_]): Infix_>> = new Infix_>>(x)

    sealed class For[a](x: m[a]) {
        def flatMap[b](y: a => m[b]): m[b] = op_>>=(x)(y)
        def map[b](y: a => b): m[b] = op_>>=(x)(_x => `return`(y(_x)))
        def filter(y: a => Bool): m[a] = map(_x => seq(Predef.require(y(_x)))(_x))
        def withFilter(y: a => Bool): m[a] = filter(y)
    }
    final implicit def `for`[a](x: m[a]): For[a] = new For(x)

    sealed class Infix_=<<[a, b](f: a => m[b]) {
        def =<<(x: m[a]): m[b] = op_=<<(f)(x)
    }
    final implicit def =<<[a, b](f: a => m[b]): Infix_=<<[a, b] = new Infix_=<<[a, b](f)
}


trait MonadProxy[m[+_]] extends Monad[m] with ApplicativeProxy[m] {
    override def self: Monad[m]
    override def `return`[a](x: a): m[a] = self.`return`(x)
    override def op_>>=[a, b](x: m[a])(y: a => m[b]): m[b] = self.op_>>=(x)(y)
    override def op_>>[b](x: m[_])(y: => m[b]): m[b] = self.op_>>(x)(y)
}


object Monad {
    def apply[m[+_]](implicit i: Monad[m]): Monad[m] = i

    implicit val ofIdentity: Monad[({type m[+a] = a})#m] = Identity.monad
    implicit def ofFunction[r]: Monad[({type m[+a] = r => a})#m] = Function.monad[r]
}
