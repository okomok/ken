

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Foldable[t[+_]] extends TypeClass1[t] {
    final val asFoldable: Foldable[apply] = this

    // Core
    //
    def fold[m](f: t[m])(implicit i: Monoid[m]): m = foldMap(id[m])(f)(i)
    def foldMap[a, m](f: a => m)(x: t[a])(implicit i: Monoid[m]): m = foldr[a, m](i.mappend compose f)(i.mempty)(x)

    def foldr[a, b](f: a => (=> b) => b)(z: b)(t: t[a]): b = {
        import ByName._
        foldMap[a, b => b](f)(t)(Endo.weak[b].asMonoid)(z)
    }

    def foldl[a, b](f: a => b => a)(z: a)(t: t[b]): a = foldMap(flip(f))(t)(Endo.weak[a].asMonoid.dual)(z)

    def foldr1[a](f: a => (=> a) => a)(xs: t[a]): a = {
        def mf(x: a)(y: => Maybe[a]): Maybe[a] = y match {
            case Nothing => Just(x)
            case Just(y) => Just(f(x)(y))
        }
        Maybe.fromMaybe(error("foldr1: empty structure"))(foldr(mf)(Nothing)(xs))
    }

    def foldl1[a](f: a => a => a)(xs: t[a]):a = {
        def mf(x: Maybe[a])(y: a): Maybe[a] = x match {
            case Nothing => Just(y)
            case Just(x) => Just(f(x)(y))
        }
        Maybe.fromMaybe(error("foldl1: empty structure"))(foldl(mf)(Nothing)(xs))
    }

    // Extra
    //
    def foldr_[a, b](f: a => b => b)(z0: b)(xs: t[a]): b = {
        def f_(k: b => b)(x: a)(z: b): b = k { f(x)(z) }
        foldl[b => b, a](f_)(id)(xs)(z0)
    }

    def foldrM[m[+_], a, b](f: a => (=> b) => m[b])(z0: b)(xs: t[a])(implicit i: Monad[m]): m[b] = {
        // not lazy
        import i.>>=
        import ByName._
        def f_(k: (=> b) => m[b])(x: a)(z: => b): m[b] = f(x)(z) >>= k
        foldl(f_)(i.`return`)(xs)(z0)
    }

    def foldlM[m[+_], a, b](f: a => b => m[a])(z0: a)(xs: t[b])(implicit i: Monad[m]): m[a] = {
        import i.>>=
        import ByName._
        def f_(x: b)(k: a => m[a])(z: a): m[a] = f(z)(x) >>= k
        foldr_(f_)(i.`return`[a])(xs)(z0)
    }

    // *> is equivalent to >> ?

    def traverse_[f[+_], a, b](f: a => f[b])(xs: t[a])(implicit i: Applicative[f]): f[Unit] = {
        foldr_(i.op_*>[b, Unit]_ compose f)(i.pure())(xs)
    }

    def for_[f[+_], a, b](xs: t[a])(f: a => f[b])(implicit i: Applicative[f]): f[Unit] = traverse_(f)(xs)

    def mapM_[m[+_], a, b](f: a => m[b])(xs: t[a])(implicit i: Monad[m]): m[Unit] = {
        foldr(i.op_>>[Unit]_ compose f)(i.`return`())(xs)
    }

    def forM_[m[+_], a, b](xs: t[a])(f: a => m[b])(implicit i: Monad[m]): m[Unit] = mapM_(f)(xs)

    def sequenceA_[f[+_], a](xs: t[f[a]])(implicit i: Applicative[f]): f[Unit] = {
        foldr_(i.op_*>[a, Unit])(i.pure())(xs)
    }

    def sequence_[m[+_], a](xs: t[m[a]])(implicit i: Monad[m]): m[Unit] = {
        foldr(i.op_>>[Unit])(i.`return`())(xs)
    }

    def asum[f[+_], a](xs: t[f[a]])(implicit i: Alternative[f]): f[a] = {
        foldr(i.op_<|>[a])(i.empty)(xs)
    }

    def msum[m[+_], a](xs: t[m[a]])(implicit i: MonadPlus[m]): m[a] = {
        foldr(i.mplus[a])(i.mzero)(xs)
    }

    def toList[a](xs: t[a]): List[a] = {
        foldr(List.op_::[a])(Nil)(xs)
    }

    def concat[a](xs: t[List[a]]): List[a] = fold(xs)
    def concatMap[a, b](f: a => List[b])(xs: t[a]): List[b] = foldMap(f)(xs)

    def and(xs: t[Bool]): Bool = foldMap(id[Bool])(xs)(Monoid.All.weak.asMonoid)
    def or(xs: t[Bool]): Bool = foldMap(id[Bool])(xs)(Monoid.Any_.weak.asMonoid)

    def any[a](p: a => Bool)(xs: t[a]): Bool = foldMap(p)(xs)(Monoid.Any_.weak.asMonoid)
    def all[a](p: a => Bool)(xs: t[a]): Bool = foldMap(p)(xs)(Monoid.All.weak.asMonoid)

    def sum[a](xs: t[a])(implicit i: Num[a]): a = foldMap(id[a])(xs)(Monoid.Sum.weak.asMonoid)
    def product[a](xs: t[a])(implicit i: Num[a]): a = foldMap(id[a])(xs)(Monoid.Product.weak.asMonoid)

    def maximum[a](xs: t[a])(implicit i: Ord[a]): a = foldl1(i.max)(xs)

    def maximumBy[a](cmp: a => a => Ordering)(xs: t[a]): a = {
        def max_(x: a)(y: a): a = cmp(x)(y) match {
            case GT => x
            case _ => y
        }
        foldl1(max_)(xs)
    }

    def minimum[a](xs: t[a])(implicit i: Ord[a]): a = foldl1(i.min)(xs)

    def minimumBy[a](cmp: a => a => Ordering)(xs: t[a]): a = {
        def min_(x: a)(y: a): a = cmp(x)(y) match {
            case GT => y
            case _ => x
        }
        foldl1(min_)(xs)
    }

    def elem[a](x: a)(xs: t[a]): Bool = any(Eq[a].op_==(x))(xs)
    def notElem[a](x: a)(xs: t[a]): Bool = not(elem(x)(xs))

    def find[a](p: a => Bool)(xs: t[a]): Maybe[a] = {
        Maybe.listToMaybe(concatMap((x: a) => if (p(x)) List(x) else Nil)(xs))
    }
}


trait FoldableProxy[t[+_]] extends Foldable[t] with Proxy {
    override def self: Foldable[t]

    override def fold[m](f: t[m])(implicit i: Monoid[m]): m = self.fold(f)(i)
    override def foldMap[a, m](f: a => m)(x: t[a])(implicit i: Monoid[m]): m = self.foldMap(f)(x)(i)
    override def foldr[a, b](f: a => (=> b) => b)(z: b)(t: t[a]): b = self.foldr(f)(z)(t)
    override def foldl[a, b](f: a => b => a)(z: a)(t: t[b]): a = self.foldl(f)(z)(t)
    override def foldr1[a](f: a => (=> a) => a)(xs: t[a]): a = self.foldr1(f)(xs)
    override def foldl1[a](f: a => a => a)(xs: t[a]):a = self.foldl1(f)(xs)

    override def foldr_[a, b](f: a => b => b)(z0: b)(xs: t[a]): b = self.foldr_(f)(z0)(xs)
    override def foldrM[m[+_], a, b](f: a => (=> b) => m[b])(z0: b)(xs: t[a])(implicit i: Monad[m]): m[b] = self.foldrM(f)(z0)(xs)(i)
    override def foldlM[m[+_], a, b](f: a => b => m[a])(z0: a)(xs: t[b])(implicit i: Monad[m]): m[a] = self.foldlM(f)(z0)(xs)(i)
    override def traverse_[f[+_], a, b](f: a => f[b])(xs: t[a])(implicit i: Applicative[f]): f[Unit] = self.traverse_(f)(xs)(i)
    override def for_[f[+_], a, b](xs: t[a])(f: a => f[b])(implicit i: Applicative[f]): f[Unit] = self.for_(xs)(f)(i)
    override def mapM_[m[+_], a, b](f: a => m[b])(xs: t[a])(implicit i: Monad[m]): m[Unit] = self.mapM_(f)(xs)(i)
    override def forM_[m[+_], a, b](xs: t[a])(f: a => m[b])(implicit i: Monad[m]): m[Unit] = self.forM_(xs)(f)(i)
    override def sequenceA_[f[+_], a](xs: t[f[a]])(implicit i: Applicative[f]): f[Unit] = self.sequenceA_(xs)(i)
    override def sequence_[m[+_], a](xs: t[m[a]])(implicit i: Monad[m]): m[Unit] = self.sequence_(xs)(i)
    override def asum[f[+_], a](xs: t[f[a]])(implicit i: Alternative[f]): f[a] = self.asum(xs)(i)
    override def msum[m[+_], a](xs: t[m[a]])(implicit i: MonadPlus[m]): m[a] = self.msum(xs)(i)
    override def toList[a](xs: t[a]): List[a] = self.toList(xs)
    override def concat[a](xs: t[List[a]]): List[a] = self.concat(xs)
    override def concatMap[a, b](f: a => List[b])(xs: t[a]): List[b] = self.concatMap(f)(xs)
    override def and(xs: t[Bool]): Bool = self.and(xs)
    override def or(xs: t[Bool]): Bool = self.or(xs)
    override def any[a](p: a => Bool)(xs: t[a]): Bool = self.any(p)(xs)
    override def all[a](p: a => Bool)(xs: t[a]): Bool = self.all(p)(xs)
    override def sum[a](xs: t[a])(implicit i: Num[a]): a = self.sum(xs)(i)
    override def product[a](xs: t[a])(implicit i: Num[a]): a = self.product(xs)(i)
    override def maximum[a](xs: t[a])(implicit i: Ord[a]): a = self.maximum(xs)(i)
    override def maximumBy[a](cmp: a => a => Ordering)(xs: t[a]): a = self.maximumBy(cmp)(xs)
    override def minimum[a](xs: t[a])(implicit i: Ord[a]): a = self.minimum(xs)(i)
    override def minimumBy[a](cmp: a => a => Ordering)(xs: t[a]): a = self.minimumBy(cmp)(xs)
    override def elem[a](x: a)(xs: t[a]): Bool = self.elem(x)(xs)
    override def notElem[a](x: a)(xs: t[a]): Bool = self.notElem(x)(xs)
    override def find[a](p: a => Bool)(xs: t[a]): Maybe[a] = self.find(p)(xs)
}


object Foldable {
    def apply[t[+_]](implicit i: Foldable[t]): Foldable[t] = i
}
