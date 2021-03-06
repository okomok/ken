

// Copyright Shunsuke Sogame 2011.
//
// Copyright 2004, The University Court of the University of Glasgow.
// All rights reserved.
//
// Copyright (c) 2002 Simon Peyton Jones
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait Monad[m[+_]] extends Applicative[m] { outer =>
    final val asMonad: Monad[apply1] = this

    // Core
    //
    def `return`[a](x: Lazy[a]): m[a] = pure(x)
    def op_>>=[a, b](x: m[a])(y: a => m[b]): m[b]
    def op_>>[b](x: m[_])(y: Lazy[m[b]]): m[b] = x >>= (_ => y)

    // Overrides
    //
    // Applicative
    private type f[+a] = m[a]
    override def pure[a](x: Lazy[a]): f[a] = `return`(x)
    override def op_<*>[a, b](x: f[a => b]): f[a] => f[b] = y => {
        x >>= { _x =>
            y >>= { _y =>
                `return`(_x(_y))
            }
        }

        // StackOverflow; `for` depends on `fmap` which depends on `<*>` by default.
        // for { _x <- x; _y <- y } yield _x(_y)
    }

    // Extra
    //
    def op_=<<:[a, b](f: a => m[b])(x: m[a]): m[b] = x >>= f

    def sequence[a](ms: List[m[a]]): m[List[a]] = {
        def k(m: m[a])(_m: Lazy[m[List[a]]]): m[List[a]] = for { x <- m; xs <- _m.! } yield (x :: xs)
        List.foldr(k)(`return`(Nil))(ms)
    }

    def sequence_[a](ms: List[m[a]]): m[Unit] = {
        List.foldr(op_>>[Unit])(`return`(()))(ms)
    }

    def mapM[a, b](f: a => m[b])(as: List[a]): m[List[b]] = sequence(List.map(f)(as))
    def mapM_[a, b](f: a => m[b])(as: List[a]): m[Unit] = sequence_(List.map(f)(as))

    def filterM[a](p: a => m[Bool])(xs: List[a]): m[List[a]] = xs match {
        case Nil => `return`(Nil)
        case x :: xs => for {
            flg <- p(x)
            ys <- filterM(p)(xs.!)
        } yield (if (flg) (x :: ys) else ys)
    }

    def forM[a, b](xs: List[a])(f: a => m[b]): m[List[b]] = mapM(f)(xs)
    def forM_[a, b](xs: List[a])(f: a => m[b]): m[Unit] = mapM_(f)(xs)

    def op_>=>:[a, b, c](f: a => m[b])(g: b => m[c]): a => m[c] = { x => f(x) >>= g }
    def op_<=<:[a, b, c](g: b => m[c])(f: a => m[b]): a => m[c] = op_>=>:(f)(g)

    def forever[a](a: m[a]): m[a] = a >>= (_ => forever(a))

    def join[a](x: m[m[a]]): m[a] = x >>= id

    def mapAndUnzipM[a, b, c](f: a => m[(b, c)])(xs: List[a]): m[(List[b], List[c])] = {
        mapM(f)(xs) >>= (ys => `return`(List.unzip(ys)))
    }

    def zipWithM[a, b, c](f: a => b => m[c])(xs: List[a])(ys: List[b]): m[List[c]] = sequence(List.zipWith(f)(xs)(ys))
    def zipWithM_[a, b, c](f: a => b => m[c])(xs: List[a])(ys: List[b]): m[Unit] = sequence_(List.zipWith(f)(xs)(ys))

    def foldM[a, b](f: a => b => m[a])(a: a)(xs: List[b]): m[a] = xs match {
        case Nil => `return`(a)
        case x :: xs => f(a)(x) >>= (fax => foldM(f)(fax)(xs.!))
    }

    def foldM_[a, b](f: a => b => m[a])(a: a)(xs: List[b]): m[Unit] = {
        foldM(f)(a)(xs) >> `return`(())
    }

    def replicateM[a](n: Int)(x: m[a]): m[List[a]] = sequence(List.replicate(n)(x))
    def replicateM_[a](n: Int)(x: m[a]): m[Unit] = sequence_(List.replicate(n)(x))

    def when(p: Bool)(s: Lazy[m[Unit]]): m[Unit] = if (p) s else `return`()
    def unless(p: Bool)(s: Lazy[m[Unit]]): m[Unit] = if (p) `return`() else s

    def liftM[a1, r](f: a1 => r)(m1: m[a1]): m[r] = for { x1 <- m1 } yield f(x1)
    def liftM2[a1, a2, r](f: a1 => a2 => r)(m1: m[a1])(m2: m[a2]): m[r] = for { x1 <- m1; x2 <- m2 } yield f(x1)(x2)
    def liftM3[a1, a2, a3, r](f: a1 => a2 => a3 => r)(m1: m[a1])(m2: m[a2])(m3: m[a3]): m[r] = for { x1 <- m1; x2 <- m2; x3 <- m3 } yield f(x1)(x2)(x3)
    def liftM4[a1, a2, a3, a4, r](f: a1 => a2 => a3 => a4 => r)(m1: m[a1])(m2: m[a2])(m3: m[a3])(m4: m[a4]): m[r] = for { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4 } yield f(x1)(x2)(x3)(x4)
    def liftM5[a1, a2, a3, a4, a5, r](f: a1 => a2 => a3 => a4 => a5 => r)(m1: m[a1])(m2: m[a2])(m3: m[a3])(m4: m[a4])(m5: m[a5]): m[r] = for { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5 } yield f(x1)(x2)(x3)(x4)(x5)

    def ap[a, b](x: m[a => b])(y: m[a]): m[b] = liftM2(id[a => b])(x)(y) // op_<*>(x)(y)

    // Operators
    //
    private[ken] sealed class Op_>>=[a](x: m[a]) {
        def >>=[b](y: a => m[b]): m[b] = op_>>=(x)(y)
    }
    final implicit def >>=[a](x: m[a]): Op_>>=[a] = new Op_>>=(x)

    private[ken] sealed class Op_>>(x: m[_]) {
        def >>[b](y: m[b]): m[b] = op_>>(x)(y)
    }
    final implicit def >>(x: m[_]): Op_>> = new Op_>>(x)

    private[ken] sealed class Op_=<<:[a](x: m[a]) {
        def =<<:[b](f: a => m[b]): m[b] = op_=<<:(f)(x)
    }
    final implicit def =<<:[a](x: m[a]): Op_=<<:[a] = new Op_=<<:(x)

    private[ken] sealed class Op_>=>:[b, c](g: b => m[c]) {
        def >=>:[a](f: a => m[b]): a => m[c] = op_>=>:(f)(g)
    }
    final implicit def >=>:[b, c](g: b => m[c]): Op_>=>:[b, c] = new Op_>=>:(g)

    private[ken] sealed class Op_<=<:[a, b](f: a => m[b]) {
        def <=<:[c](g: b => m[c]): a => m[c] = op_<=<:(g)(f)
    }
    final implicit def <=<:[a, b](f: a => m[b]): Op_<=<:[a, b] = new Op_<=<:(f)

    // For-comprehension
    //
    private[this] def functorFor[a](m: m[a]): For[a] = super.`for`(m)

    // @scalacWorkaround("2.9.1", 5070)
    override implicit def `for`[a](m: m[a]): ken.For[m, a] = new ForProxy[a] {
        override val selfFor: selfFor = functorFor(m)
        override def flatMap[b](k: a => m[b]): m[b] = outer.op_>>=(m)(k)
    }
}


trait MonadProxy[m[+_]] extends Monad[m] with ApplicativeProxy[m] {
    type selfMonad = Monad[m]
    def selfMonad: selfMonad
    override def selfApplicative: selfApplicative = selfMonad

    override def `return`[a](x: Lazy[a]): m[a] = selfMonad.`return`(x)
    override def op_>>=[a, b](x: m[a])(y: a => m[b]): m[b] = selfMonad.op_>>=(x)(y)
    override def op_>>[b](x: m[_])(y: Lazy[m[b]]): m[b] = selfMonad.op_>>(x)(y)

    override def op_=<<:[a, b](f: a => m[b])(x: m[a]): m[b] = selfMonad.op_=<<:(f)(x)
    override def sequence[a](ms: List[m[a]]): m[List[a]] = selfMonad.sequence(ms)
    override def sequence_[a](ms: List[m[a]]): m[Unit] = selfMonad.sequence_(ms)
    override def mapM[a, b](f: a => m[b])(as: List[a]): m[List[b]] = selfMonad.mapM(f)(as)
    override def mapM_[a, b](f: a => m[b])(as: List[a]): m[Unit] = selfMonad.mapM_(f)(as)
    override def filterM[a](p: a => m[Bool])(xs: List[a]): m[List[a]] = selfMonad.filterM(p)(xs)
    override def forM[a, b](xs: List[a])(f: a => m[b]): m[List[b]] = selfMonad.forM(xs)(f)
    override def forM_[a, b](xs: List[a])(f: a => m[b]): m[Unit] = selfMonad.forM_(xs)(f)
    override def op_>=>:[a, b, c](f: a => m[b])(g: b => m[c]): a => m[c] = selfMonad.op_>=>:(f)(g)
    override def op_<=<:[a, b, c](g: b => m[c])(f: a => m[b]): a => m[c] = selfMonad.op_<=<:(g)(f)
    override def forever[a](a: m[a]): m[a] = selfMonad.forever(a)
    override def join[a](x: m[m[a]]): m[a] = selfMonad.join(x)
    override def mapAndUnzipM[a, b, c](f: a => m[(b, c)])(xs: List[a]): m[(List[b], List[c])] = selfMonad.mapAndUnzipM(f)(xs)
    override def zipWithM[a, b, c](f: a => b => m[c])(xs: List[a])(ys: List[b]): m[List[c]] = selfMonad.zipWithM(f)(xs)(ys)
    override def zipWithM_[a, b, c](f: a => b => m[c])(xs: List[a])(ys: List[b]): m[Unit] = selfMonad.zipWithM_(f)(xs)(ys)
    override def foldM[a, b](f: a => b => m[a])(a: a)(xs: List[b]): m[a] = selfMonad.foldM(f)(a)(xs)
    override def foldM_[a, b](f: a => b => m[a])(a: a)(xs: List[b]): m[Unit] = selfMonad.foldM_(f)(a)(xs)
    override def replicateM[a](n: Int)(x: m[a]): m[List[a]] = selfMonad.replicateM(n)(x)
    override def replicateM_[a](n: Int)(x: m[a]): m[Unit] = selfMonad.replicateM_(n)(x)
    override def when(p: Bool)(s: Lazy[m[Unit]]): m[Unit] = selfMonad.when(p)(s)
    override def unless(p: Bool)(s: Lazy[m[Unit]]): m[Unit] = selfMonad.unless(p)(s)
    override def liftM[a1, r](f: a1 => r)(m1: m[a1]): m[r] = selfMonad.liftM(f)(m1)
    override def liftM2[a1, a2, r](f: a1 => a2 => r)(m1: m[a1])(m2: m[a2]): m[r] = selfMonad.liftM2(f)(m1)(m2)
    override def liftM3[a1, a2, a3, r](f: a1 => a2 => a3 => r)(m1: m[a1])(m2: m[a2])(m3: m[a3]): m[r] = selfMonad.liftM3(f)(m1)(m2)(m3)
    override def liftM4[a1, a2, a3, a4, r](f: a1 => a2 => a3 => a4 => r)(m1: m[a1])(m2: m[a2])(m3: m[a3])(m4: m[a4]): m[r] = selfMonad.liftM4(f)(m1)(m2)(m3)(m4)
    override def liftM5[a1, a2, a3, a4, a5, r](f: a1 => a2 => a3 => a4 => a5 => r)(m1: m[a1])(m2: m[a2])(m3: m[a3])(m4: m[a4])(m5: m[a5]): m[r] = selfMonad.liftM5(f)(m1)(m2)(m3)(m4)(m5)
    override def ap[a, b](x: m[a => b])(y: m[a]): m[b] = selfMonad.ap(x)(y)
}


object Monad {
    def apply[m <: Kind.Function1](implicit i: Monad[m#apply1]): Monad[m#apply1] = i

    def deriving[nt <: Kind.Newtype1](implicit j: Newtype1[nt#apply1, nt#oldtype1], i: Monad[nt#oldtype1]): Monad[nt#apply1] = new Monad[nt#apply1] with ApplicativeProxy[nt#apply1] {
        private type m[+a] = nt#apply1[a]
        override val selfApplicative: selfApplicative = Applicative.deriving[nt]

        override def `return`[a](x: Lazy[a]): m[a] = j.newOf { i.`return`(x) }
        override def op_>>=[a, b](x: m[a])(y: a => m[b]): m[b] = j.newOf { i.op_>>=(j.oldOf(x))(a => j.oldOf(y(a))) }
        override def op_>>[b](x: m[_])(y: Lazy[m[b]]): m[b] = j.newOf { i.op_>>(j.oldOf(x: m[Any]))(j.oldOf(y)) }

        // TODO
    }

    def weak[nt <: Kind.Newtype1](implicit j: Newtype1[nt#apply1, nt#oldtype1], i: Monad[nt#apply1]): Monad[nt#oldtype1] = deriving[Kind.coNewtype1[nt]](j.coNewtype, i)
}
