

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


import scala.annotation.tailrec


sealed abstract class List[+a] extends Up[List[a]] {
    final def of[b >: a]: List[b] = this

    final override def toString: JString = {
        if (this eq Nil) "Nil"
        else if (List.all((_: a).isInstanceOf[Char])(this)) toScalaList.mkString("\"", "", "\"")
        else toScalaList.mkString("List(", ",", ")")
    }

    final override def hashCode: Int = toScalaList.hashCode

    final def !!(n: Int): a = List.op_!!(this)(n)
    final def !::[b >: a](x: b): List[b] = List.op_!::[b](x)(this)

    final def \\[b >: a](that: List[b])(implicit j: Eq[b]): List[b] = List.op_\\[b](this)(that)

    final def asJString[b >: a](implicit ev: List[b] =:= String): JString = {
        List.foldl((sb: StringBuilder) => (c: Char) => sb += c)(new StringBuilder)(ev(this)).toString
    }

    final def breakOut[To](implicit bf: scala.collection.generic.CanBuildFrom[Nothing, a, To]): To = {
        List.foldl((b: scala.collection.mutable.Builder[a, To]) => (x: a) => b += x)(bf())(this).result
    }

    final def toScalaList: scala.List[a] = breakOut[scala.List[a]]
    final def toScalaStream: scala.Stream[a] = breakOut[scala.Stream[a]]
}


case object Nil extends List[Nothing] {
    def ::[a](x: a): List[a] = new ::[a](x, Lazy(this))
}

final case class ::[+a](head: a, tail: Lazy[List[a]]) extends List[a] {
    override def equals(that: Any): Bool = that match {
        case that: List[_] => List._asEq(Eq.of[Any]).op_===(this)(that)
        case _ => False
    }
}


/**
 * Strict extractor
 */
object !:: {
    def unapply[a](xs: List[a]): Option[(a, List[a])] = xs match {
        case Nil => None
        case x :: xs => Some(x, xs.!)
    }
}


object List extends ListAs with MonadPlus[List] with Traversable[List] with Extend[List] with ThisIsInstance {
    def op_::[a](x: a)(xs: Lazy[List[a]]): List[a] = ::(x, Lazy(xs))
    def op_!::[a](x: a)(xs: List[a]): List[a] = op_::(x)(xs)

    sealed class _OfName[a](xs: Lazy[List[a]]) {
        def ::(x: a): List[a] = op_::(x)(xs)
        def ++:(ys: List[a]): List[a] = op_++:(ys)(xs)
    }
    implicit def _ofName[a](xs: => List[a]): _OfName[a] = new _OfName(xs)

    // Overrides
    //
    // Functor
    private type f[+a] = List[a]
    override def fmap[a, b](f: a => b): f[a] => f[b] = xs => map(f)(xs)
    // Monad
    private type m[+a] = List[a]
    override def `return`[a](x: Lazy[a]): m[a] = List(x)
    override def op_>>=[a, b](x: m[a])(y: a => m[b]): m[b] = concat(map(y)(x))
    // MonadPlus
    override def mzero: m[Nothing] = Nil
    override def mplus[a](x: m[a])(y: Lazy[m[a]]): m[a] = x ++: y.!
    // Foldable
    private type t[+a] = List[a]
    override def toList[a](xs: t[a]): List[a]  = xs
    // Traversable
    override def traverse[f[+_], a, b](f: a => f[b])(t: t[a])(implicit i: Applicative[f]): f[t[b]] = {
        import i.{<@>, <*>}
        def cons_f(x: a)(ys: Lazy[f[t[b]]]): f[t[b]] = Function.from(op_!::[b]) <@> f(x) <*> ys
        foldr(cons_f)(i.pure(Nil))(t)
    }
    override def mapM[m_[+_], a, b](f: a => m_[b])(t: t[a])(implicit i: Monad[m_]): m_[t[b]] = i.mapM(f)(t)
    // Extend
    private type w[+a] = List[a]
    override def duplicate[a](w: w[a]): w[w[a]] = List.tails(w)

    // Conversions
    //
    def from[a](that: List[a]): List[a] = that

    def apply[a](xs: a*): List[a] = from(xs)
    def unapplySeq[a](xs: List[a]): Option[Seq[a]] = Some(xs.toScalaList)

    implicit def fromIterator[a](it: scala.Iterator[a]): List[a] = {
        if (it.hasNext) {
            val x = it.next
            x :: fromIterator(it)
        } else {
            Nil
        }
    }

    implicit def fromArray[a](xs: Array[a]): List[a] = fromIterable(xs)
    implicit def fromString(xs: JString): String = fromIterable(xs)
    implicit def fromIterable[a](xs: scala.Iterable[a]): List[a] = fromIterator(xs.iterator)

    // Basic functions
    //
    def op_++:[a](xs: List[a])(ys: Lazy[List[a]]): List[a] = xs match {
        case Nil => ys
        case x :: xs => x :: op_++:(xs.!)(ys)
    }

    def op_!++:[a](xs: List[a])(ys: List[a]): List[a] = op_++:(xs)(ys)

    def head[a](xs: List[a]): a = xs match {
        case Nil => error("empty List")
        case x :: _ => x
    }

    @tailrec
    def last[a](xs: List[a]): a = xs match {
        case Nil => error("empty List")
        case x !:: Nil => x
        case _ :: xs => last(xs.!)
    }

    def tail[a](xs: List[a]): List[a] = xs match {
        case Nil => error("empty List")
        case _ :: xs => xs.!
    }

    def init[a](xs: List[a]): List[a] = xs match {
        case Nil => error("empty List")
        case x !:: Nil => Nil
        case x :: xs => x :: init(xs.!)
    }

    def `null`(xs: List[_]): Bool = xs match {
        case Nil => True
        case _ => False
    }

    def length(xs: List[_]): Int = {
        @tailrec
        def len(l: List[_])(a: Int): Int = l match {
            case Nil => a
            case x :: xs => len(xs.!)(a + 1)
        }
        len(xs)(0)
    }

    // List transformations
    //
    def map[a, b](f: a => b)(xs: List[a]): List[b] = xs match {
        case Nil => Nil
        case x :: xs => f(x) :: map(f)(xs.!)
    }

    def reverse[a](xs: List[a]): List[a] = foldl(flip(op_!::[a]))(Nil)(xs)

    def intersperse[a](sep: a)(xs: List[a]): List[a] = xs match {
        case Nil => Nil
        case x !:: Nil => List(x)
        case x :: xs => x :: sep :: intersperse(sep)(xs.!)
    }

    def intercalate[a](xs: List[a])(xss: List[List[a]]): List[a] = concat(intersperse(xs)(xss))

    def transpose[a](xss: List[List[a]]): List[List[a]] = xss match {
        case Nil => Nil
        case Nil :: xss => transpose(xss.!)
        case (x :: xs) :: xss => (x :: (for { h :: _ <- xss.!} yield h)) :: transpose(xs.! :: (for { _ :: t <- xss.! } yield t.!))
    }

    def subsequences[a](xs: List[a]): List[List[a]] = Nil :: (nonEmptySubsequences(xs))

    def nonEmptySubsequences[a](xs: List[a]): List[List[a]] = xs match {
        case Nil => Nil
        case x :: xs => {
            def f(ys: List[a])(r: Lazy[List[List[a]]]): List[List[a]] = ys :: (x :: ys) :: r.!
            (x :: Nil) :: foldr(f)(Nil)(nonEmptySubsequences(xs.!))
        }
    }

    def permutations[a](xs0: List[a]): List[List[a]] = {
        def perms(ts: List[a])(is: List[a]): List[List[a]] = ts match {
            case Nil => Nil
            case t :: ts => {
                def interleave(xs: List[a])(r: Lazy[List[List[a]]]): List[List[a]] = interleave_(id)(xs)(r)._2
                def interleave_(f: List[a] => List[a])(ys: List[a])(r: List[List[a]]): (List[a], List[List[a]]) = ys match {
                    case Nil => (ts.!, r)
                    case y :: ys => {
                        lazy val uzs = interleave_(f `.` (y :: _))(ys.!)(r)
                        (y :: uzs._1, f(t :: y :: uzs._1) :: uzs._2)
                    }
                }
                foldr(interleave)(perms(ts.!)(t :: is))(permutations(is))
            }
        }
        xs0 :: perms(xs0)(Nil)
    }

    // Reducing lists (folds)
    //
    override def foldl[a, b](f: a => b => a)(z: a)(xs: List[b]): a = {
        @tailrec
        def impl(f: a => b => a)(z: a)(xs: List[b]): a = xs match {
            case Nil => z
            case x :: xs => impl(f)(f(z)(x))(xs.!)
        }
        impl(f)(z)(xs)
    }

    override def foldl1[a](f: a => a => a)(xs: List[a]): a = xs match {
        case Nil => error("empty List")
        case x :: xs => foldl(f)(x)(xs.!)
    }

    override def foldr[a, b](f: a => Lazy[b] => b)(z: b)(xs: List[a]): b = xs match {
        case Nil => z
        case x :: xs => f(x)(foldr(f)(z)(xs.!))
    }

    override def foldr1[a](f: a => Lazy[a] => a)(xs: List[a]): a = xs match {
        case Nil => error("empty List")
        case x !:: Nil => x
        case x :: xs => f(x)(foldr1(f)(xs.!))
    }

    // Special folds
    //
    override def concat[a](xs: List[List[a]]): List[a] = foldr(op_++:[a])(Nil)(xs)

    override def concatMap[a, b](f: a => List[b])(xs: List[a]): List[b] = foldr[a, List[b]](x => y => f(x) ++: y.!)(Nil)(xs)

    override def and(xs: List[Bool]): Bool = foldr(Bool.op_&&)(True)(xs)

    override def or(xs: List[Bool]): Bool = foldr(Bool.op_||)(False)(xs)

    override def any[a](p: a => Bool)(xs: List[a]): Bool = or(map(p)(xs))

    override def all[a](p: a => Bool)(xs: List[a]): Bool = and(map(p)(xs))

    override def sum[a](xs: List[a])(implicit i: Num[a]): a = foldl(i.op_+)(i.fromIntegral(0))(xs)

    override def product[a](xs: List[a])(implicit i: Num[a]): a = foldl(i.op_*)(i.fromIntegral(1))(xs)

    override def maximum[a](xs: List[a])(implicit i: Ord[a]): a = xs match {
        case Nil => error("empty List")
        case xs => foldl1(i.max)(xs)
    }

    override def minimum[a](xs: List[a])(implicit i: Ord[a]): a = xs match {
        case Nil => error("empty List")
        case xs => foldl1(i.min)(xs)
    }

    // Building lists
    //
    def scanl[a, b](f: a => b => a)(q: Lazy[a])(ls: List[b]): List[a] = { // why `q` is by-name?
        q :: (ls match {
            case Nil => Nil
            case x :: xs => scanl(f)(f(q)(x))(xs.!)
        })
    }

    def scanl1[a](f: a => a => a)(xs: List[a]): List[a] = xs match {
        case Nil => error("empty List")
        case x :: xs => scanl(f)(x)(xs.!)
    }

    def scanr[a, b](f: a => Lazy[b] => b)(q0: b)(xs: List[a]): List[b] = xs match {
        case Nil => List(q0)
        case x :: xs => {
            lazy val qs: List[b] = scanr(f)(q0)(xs.!)
            f(x)(Lazy(head(qs))) :: qs
        }
    }

    def scanr1[a](f: a => Lazy[a] => a)(xs: List[a]): List[a] = xs match {
        case Nil => Nil
        case x !:: Nil => List(x)
        case x :: xs => {
            lazy val qs = scanr1(f)(xs.!)
            f(x)(Lazy(head(qs))) :: qs
        }
    }

    // Accumulating maps
    //
    override def mapAccumL[acc, x, y](f: acc => x => (acc, y))(s: acc)(xs: List[x]): (acc, List[y]) = xs match {
        case Nil => (s, Nil)
        case x :: xs => {
            val (s_, y) = f(s)(x)
            val (s__, ys) = mapAccumL(f)(s_)(xs.!)
            (s__, y :: ys)
        }
    }

    override def mapAccumR[acc, x, y](f: Lazy[acc] => x => (acc, y))(s: acc)(xs: List[x]): (acc, List[y]) = xs match {
        case Nil => (s, Nil)
        case x :: xs => {
            lazy val s_ys = mapAccumR(f)(s)(xs.!)
            val (s__, y) = f(s_ys._1)(x)
            (s__, y :: s_ys._2)
        }
    }

    // Infinite lists
    //
    def iterate[a](f: a => a)(x: a): List[a] = x :: iterate(f)(f(x))

    def repeat[a](x: a): List[a] = {
        // `lazy val` works around "forward reference extends over definition of value xs".
        // Notice `def xs` falls into space-leak.
        lazy val xs: List[a] = x :: xs
        xs
    }

    def replicate[a](n: Int)(x: a): List[a] = take(n)(repeat(x))

    def cycle[a](xs: List[a]): List[a] = xs match {
        case Nil => error("empty List")
        case xs => {
            lazy val xs_ : List[a] = xs ++: xs_
            xs_
        }
    }

    // Unfolding
    //
    def unfoldr[a, b](f: b => Maybe[(a, b)])(b: b): List[a] = f(b) match {
        case Just((a, new_b)) => a :: unfoldr(f)(new_b)
        case Nothing => Nil
    }

    // Sublists
    //
    def take[a](n: Int)(xs: List[a]): List[a] = (n, xs) match {
        case (n, _) if n <= 0 => Nil
        case (_, Nil) => Nil
        case (n, x :: xs) => x :: take(n-1)(xs.!)
    }

    @tailrec
    def drop[a](n: Int)(xs: List[a]): List[a] = (n, xs) match {
        case (n, xs) if n <= 0 => xs
        case (_, Nil) => Nil
        case (n, _ :: xs) => drop(n-1)(xs.!)
    }

    def splitAt[a](n: Int)(xs: List[a]): (List[a], List[a]) = (take(n)(xs), drop(n)(xs))

    def takeWhile[a](p: a => Bool)(xs: List[a]): List[a] = xs match {
        case Nil => Nil
        case x :: xs => if (p(x)) x :: takeWhile(p)(xs.!) else Nil
    }

    @tailrec
    def dropWhile[a](p: a => Bool)(xs: List[a]): List[a] = xs match {
        case Nil => Nil
        case x :: xs_ => if (p(x)) dropWhile(p)(xs_.!) else xs
    }

    def span[a](p: a => Bool)(xs: List[a]): (List[a], List[a]) = xs match {
        case Nil => (Nil, Nil)
        case x :: xs_ => {
            if (p(x)) {
                val (ys, zs) = span(p)(xs_.!)
                (x :: ys, zs)
            } else {
                (Nil, xs)
            }
        }
    }

    def break[a](p: a => Bool)(xs: List[a]): (List[a], List[a]) = span[a](!p(_))(xs)

    @tailrec
    def stripPrefix[a](xs: List[a])(ys: List[a])(implicit j: Eq[a]): Maybe[List[a]] = (xs, ys) match {
        case (Nil, ys) => Just(ys)
        case (x :: xs, y :: ys) if x === y => stripPrefix(xs.!)(ys.!)
        case _ => Nothing
    }

    def group[a](xs: List[a])(implicit j: Eq[a]): List[List[a]] = groupBy(op_===[a])(xs)

    def inits[a](xs: List[a]): List[List[a]] = xs match {
        case Nil => List(Nil)
        case x :: xs => List(Nil) ++: map(op_!::(x))(inits(xs.!))
    }

    def tails[a](xxs: List[a]): List[List[a]] = xxs match {
        case Nil => List(Nil)
        case _ :: xs => xxs :: tails(xs.!)
    }

    // Predicates
    //
    def isPrefixOf[a](xs: List[a])(ys: List[a]): Bool = (xs, ys) match {
        case (Nil, _) => True
        case (_, Nil) => False
        case (x :: xs, y :: ys) => (x == y) && isPrefixOf(xs.!)(ys.!)
    }

    def isSuffixOf[a](x: List[a])(y: List[a]): Bool = isPrefixOf(reverse(x))(reverse(y))

    def isInfixOf[a](needle: List[a])(haystack: List[a]): Bool = any(isPrefixOf(needle))(tails(haystack))

    // Searching by equality
    //
    override def elem[a](x: a)(xs: List[a])(implicit j: Eq[a]): Bool = any[a](x === _)(xs)

    override def notElem[a](x: a)(xs: List[a])(implicit j: Eq[a]): Bool = all[a](x /== _)(xs)

    @tailrec
    def lookup[a, b](key: a)(xs: List[(a, b)]): Maybe[b] = xs match {
        case Nil => Nothing
        case (x, y) :: xys => if (key == x) Just(y) else lookup(key)(xys.!)
    }

    // Searching with a predicate
    //
    override def find[a](p: a => Bool)(xs: List[a]): Maybe[a] = Maybe.listToMaybe(filter(p)(xs))

    override def filter[a](pred: a => Bool)(xs: List[a]): List[a] = xs match {
        case Nil => Nil
        case x :: xs => {
            if (pred(x)) {
                x :: filter(pred)(xs.!)
            } else {
                filter(pred)(xs.!)
            }
        }
    }

    def partition[a](p: a => Bool)(xs: List[a]): (List[a], List[a]) = {
        foldr(select(p))((Nil, Nil))(xs)
    }

    def select[a](p: a => Bool)(x: a)(tfs: (List[a], List[a])): (List[a], List[a]) = {
        if (p(x)) {
            (x :: tfs._1, tfs._2)
        } else {
            (tfs._1, x :: tfs._2)
        }
    }

    // Indexing lists
    //
    @tailrec
    def op_!![a](xs: List[a])(n: Int): a = (xs, n) match {
        case (_, n) if n < 0 => error("negative index")
        case (Nil, _) => error("index too large")
        case (x :: _, 0) => x
        case (_ :: xs, n) => op_!!(xs.!)(n-1)
    }

    def elemIndex[a](x: a)(xs: List[a])(implicit j: Eq[a]): Maybe[Int] = findIndex(op_===[a](x))(xs)

    def elemIndices[a](x: a)(xs: List[a])(implicit j: Eq[a]): List[Int] = findIndices(op_===[a](x))(xs)

    def findIndex[a](p: a => Bool)(xs: List[a]): Maybe[Int] = Maybe.listToMaybe(findIndices(p)(xs))

    def findIndices[a](p: a => Bool)(xs: List[a]): List[Int] = for { (x, i) <- zip(xs)(iterate[Int](_ + 1)(0)) if p(x) } yield i

    // Zipping and unzipping lists
    //
    def zip[a, b](xs: List[a])(ys: List[b]): List[(a, b)] = (xs, ys) match {
        case (a :: as, b :: bs) => (a, b) :: zip(as.!)(bs.!)
        case _ => Nil
    }

    def zipWith[a, b, c](f: a => b => c)(xs: List[a])(ys: List[b]): List[c] = (xs, ys) match {
        case (a :: as, b :: bs) => f(a)(b) :: zipWith(f)(as.!)(bs.!)
        case _ => Nil
    }

    def unzip[a, b](xs: List[(a, b)]): (List[a], List[b]) = {
        foldr[(a, b), (List[a], List[b])](ab => abs => (ab._1 :: abs._1, ab._2 :: abs._2))((Nil, Nil))(xs)
    }

    // Functions on strings
    //
    val lines: String => List[String] = s => map(fromString)(from(s.asJString.split("\\r?\\n")))

    val unlines: List[String] => String = ls => concatMap(op_!++:("\n"))(ls)

    val words: String => List[String] = s => map(fromString)(from(s.asJString.split("\\W+")))

    val unwords: List[String] => String = {
        case Nil => ""
        case w !:: Nil => w
        case w :: ws => w ++: (' ' :: unwords(ws.!))
    }

    // Set operations
    //
    def nub[a](xs: List[a])(implicit j: Eq[a]): List[a] = nubBy(op_===[a])(xs)

    def delete[a](x: a)(xs: List[a])(implicit j: Eq[a]) = deleteBy(op_===[a])(x)(xs)

    def op_\\[a](xs: List[a])(ys: List[a])(implicit j: Eq[a]): List[a] = foldl(flip(delete[a]))(xs)(ys)

    def union[a](xs: List[a])(ys: List[a])(implicit j: Eq[a]): List[a] = unionBy(op_===[a])(xs)(ys)

    def intersect[a](xs: List[a])(ys: List[a])(implicit j: Eq[a]): List[a] = intersectBy(op_===[a])(xs)(ys)

    // Ordered lists
    //
    def sort[a](xs: List[a])(implicit i: Ord[a]): List[a] = sortBy(i.compare)(xs)

    def insert[a](e: a)(ls: List[a])(implicit i: Ord[a]): List[a] = insertBy(i.compare)(e)(ls)

    // User-supplied equality
    //
    def nubBy[a](eq: a => a => Bool)(xs: List[a]): List[a] = xs match {
        case Nil => Nil
        case x :: xs => x :: nubBy(eq)(filter[a](y => Bool.not(eq(x)(y)))(xs.!))
    }

    def deleteBy[a](eq: a => a => Bool)(x: a)(xs: List[a]): List[a] = xs match {
        case Nil => Nil
        case y :: ys => if (x == y) ys.! else (y :: deleteBy(eq)(x)(ys.!))
    }

    def deleteFirstBy[a](eq: a => a => Bool)(xs: List[a])(ys: List[a]): List[a] = {
        foldl(flip(deleteBy(eq)))(xs)(ys)
    }

    def unionBy[a](eq: a => a => Bool)(xs: List[a])(ys: List[a]): List[a] = {
        xs ++: foldl(flip(deleteBy(eq)))(nubBy(eq)(ys))(xs)
    }

    def intersectBy[a](eq: a => a => Bool)(xs: List[a])(ys: List[a]): List[a] = {
        for { x <- xs if any(eq(x))(ys) } yield x
    }

    def groupBy[a](eq: a => a => Bool)(xs: List[a]): List[List[a]] = xs match {
        case Nil => Nil
        case x :: xs => {
            val (ys, zs) = span(eq(x))(xs.!)
            (x :: ys) :: groupBy(eq)(zs)
        }
    }

    // User-supplied comparison
    //
    def sortBy[a](cmp: a => a => Ordering)(xs: List[a]): List[a] = {
        foldr(insertBy(cmp))(Nil)(xs)
    }

    def insertBy[a](cmp: a => a => Ordering)(x: a)(ys: List[a]): List[a] = ys match {
        case Nil => List(x)
        case y :: ys_ => cmp(x)(y) match {
            case GT => y :: insertBy(cmp)(x)(ys_.!)
            case _ => x :: ys
        }
    }

    override def maximumBy[a](cmp: a => a => Ordering)(xs: List[a]): a = xs match {
        case Nil => error("empty List")
        case xs => {
            def maxBy(x: a)(y: a): a = cmp(x)(y) match {
                case GT => x
                case _ => y
            }
            foldl1(maxBy)(xs)
        }
    }

    override def minimumBy[a](cmp: a => a => Ordering)(xs: List[a]): a = xs match {
        case Nil => error("empty List")
        case xs => {
            def minBy(x: a)(y: a): a = cmp(x)(y) match {
                case GT => y
                case _ => x
            }
            foldl1(minBy)(xs)
        }
    }

    // The generic operations
    //
    def genericLength[i](xs: List[_])(implicit i: Num[i]): i = {
        import i._
        xs match {
            case Nil => 0
            case _ :: l => fromIntegral(1) + genericLength(l)
        }
    }

    def genericTake[i, a](n: i)(xs: List[a])(implicit i: Integral[i]): List[a] = {
        import i._
        (n, xs) match {
            case (n, _) if n <= 0 => Nil
            case (_, Nil) => Nil
            case (n, x :: xs) => x :: genericTake(n-1)(xs)
        }
    }

    def genericDrop[i, a](n: i)(xs: List[a])(implicit i: Integral[i]): List[a] = {
        import i._
        (n, xs) match {
            case (n, xs) if n <= 0 => xs
            case (_, Nil) => Nil
            case (n, _ :: xs) => genericDrop(n-1)(xs)
        }
    }

    def genericSplitAt[i, b](n: i)(xs: List[b])(implicit i: Integral[i]): (List[b], List[b]) = {
        import i._
        (n, xs) match {
            case (n, xs) if n <= 0 => (Nil, xs)
            case (_, Nil) => (Nil, Nil)
            case (n, x :: xs) => {
                val (xs_, xs__) = genericSplitAt(n-1)(xs)
                (x :: xs_, xs__)
            }
        }
    }

    def genericIndex[a, b](xs: List[b])(n: a)(implicit i: Integral[a]): b = {
        import i._
        (xs, n) match {
            case (x :: _, 0) => x
            case (_ :: xs, n) => if (n > 0) genericIndex(xs)(n-1) else error("List.genericIndex: negative argument.")
            case _ => error("List.genericIndex: index too large.")
        }
    }

    def genericReplicate[i, a](n: i)(x: a)(implicit i: Integral[i]): List[a] = genericTake(n)(repeat(x))

    // Misc
    //
    val range: Pair[Int, Int] => List[Int] = { case (n, m) =>
        Predef.require(n <= m)
        if (n == m) {
            Nil
        } else {
            n :: range(n + 1, m)
        }
    }

    val rangeFrom: Int => List[Int] = n => {
        n :: rangeFrom(n + 1)
    }

    def slice[a](n_m: (Int, Int))(xs: List[a]): List[a] = n_m match {
        case (n, m) => List.drop(n)(List.take(m)(xs))
    }

    def step[a](n: Int)(xs: List[a]): List[a] = {
        Predef.require(n > 0)
        xs match {
            case Nil => Nil
            case x :: xs => x :: List.step(n)(List.drop(n - 1)(xs.!))
        }
    }

    @tailrec
    def foreach[a](f: a => Unit)(xs: List[a]): Unit = xs match {
        case Nil => ()
        case x :: xs => {
            f(x)
            foreach(f)(xs.!)
        }
    }
}


private[ken] sealed trait ListAs { this: List.type =>
    implicit def _asEq[a](implicit i: Eq[a]): Eq[List[a]] = new Eq[List[a]] {
        override val op_=== : op_=== = {
            @tailrec
            def impl(x: List[a])(y: List[a]): Bool = (x, y) match {
                case (Nil, Nil) => True
                case (x :: xs, y :: ys) => {
                    if (i.op_/==(x)(y)) False
                    else impl(xs)(ys)
                }
                case _ => False
            }
            impl _
        }
    }

    implicit def _asOrd[a](implicit i: Ord[a]): Ord[List[a]] = new Ord[List[a]] with EqProxy[List[a]] {
        override val selfEq: selfEq = _asEq[a]
        override val compare: compare = {
            @tailrec
            def impl(x: List[a])(y: List[a]): Ordering = (x, y) match {
                case (Nil, Nil) => EQ
                case (Nil, _ :: _) => LT
                case (_ :: _, Nil) => GT
                case (x :: xs, y :: ys) => i.compare(x)(y) match {
                    case EQ => impl(xs.!)(ys.!)
                    case other => other
                }
            }
            impl _
        }
    }

    // @scalacWorkaround("2.9.1") // SI-4982
    implicit val _NilAsOrd: Ord[List[Nothing]] = new Ord[List[Nothing]] {
        override val op_=== : op_=== = x => y => True
        override val compare: compare = x => y => EQ
    }

    implicit def _asMonoid[a]: Monoid[List[a]] = new Monoid[List[a]] {
        private type m = List[a]
        override val mempty: m = Nil
        override val mappend: m => Lazy[m] => m = op_++:[a]
    }

    //implicit def _asScalaOrdering[a](implicit i: scala.Ordering[a]): scala.Ordering[List[a]] = Ord.asScalaOrdering(_asOrd[a])
}
