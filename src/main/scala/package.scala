

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok


import scala.annotation.tailrec


package object ken {

// Booleans
    def not(b: Boolean): Boolean = !b

    def op_&&(b: Boolean)(c: &[Boolean]): Boolean = b && !c

    def op_||(b: Boolean)(c: &[Boolean]): Boolean = b || !c

// Tuples
    def fst[a, b](p: (a, b)): a = p match {
        case (x, _) => x
    }

    def snd[a, b](p: (a, b)): b = p match {
        case (_, y) => y
    }

    def curry[a, b, c](f: (a, b) => c): a => b => c = { x => y => f(x, y) }

    def uncurry[a, b, c](f: a => b => c): (a, b) => c = { (x, y) => f(x)(y) }

// Miscellaneous functions
    def id[a](x: a): a = x

    def const[a](x: a): Any => a = { _ => x }

    def compose[a, b, c](f: b => c)(g: a => b): a => c = { x => f(g(x)) }

    def flip[a, b, c](f: a => b => c): b => a => c = { x => y => f(y)(x) }

    def apply[a, b](f: a => b)(x: a): b = f(x)

    def until[a](p: a => Boolean)(f: a => a)(x: a): a = {
        if (p(x)) x else until(p)(f)(f(x))
    }

    def asTypeOf[a](x: a): a => a = const(x)

    def error(msg: String): Nothing = throw new Error(msg)

    def undefined: Nothing = throw new Error("undefined")

// List operations
    def map[a, b](f: a => b)(xs: List[a]): List[b] = xs match {
        case Nil => Nil
        case x :: xs => f(x) :: map(f)(!xs)
    }

    def op_++[a](xs: List[a])(ys: List[a]): List[a] = xs match {
        case Nil => ys
        case x :: xs => x :: (!xs ++ ys)
    }

    def filter[a](pred: a => Boolean)(xs: List[a]): List[a] = xs match {
        case Nil => Nil
        case x :: xs => {
            if (pred(x)) {
                x :: filter(pred)(!xs)
            } else {
                filter(pred)(!xs)
            }
        }
    }

    def head[a](xs: List[a]): a = xs match {
        case Nil => error("empty List")
        case x :: _ => x
    }

    @tailrec
    def last[a](xs: List[a]): a = xs match {
        case Nil => error("empty List")
        case x !:: Nil => x
        case _ :: xs => last(!xs)
    }

    def tail[a](xs: List[a]): List[a] = xs match {
        case Nil => error("empty List")
        case _ :: xs => !xs
    }

    def init[a](xs: List[a]): List[a] = xs match {
        case Nil => error("empty List")
        case x !:: Nil => Nil
        case x :: xs => x :: init(!xs)
    }

    def `null`(xs: List[_]): Boolean = xs match {
        case Nil => true
        case _ => false
    }

    def length(xs: List[_]): Int = {
        @tailrec
        def len(l: List[_])(a: Int): Int = l match {
            case Nil => a
            case x :: xs => len(!xs)(a + 1)
        }
        len(xs)(0)
    }

    def op_!![a](xs: List[a])(n: Int): a = (xs, n) match {
        case (_, n) if n < 0 => error("negative index")
        case (Nil, _) => error("index too large")
        case (x :: _, 0) => x
        case (_ :: xs, n) => !xs !! (n-1)
    }

    def reverse[a](xs: List[a]): List[a] = foldl(flip(List.cons[a]))(Nil)(xs)

// Reducing lists (folds)
    @tailrec
    def foldl[a, b](f: a => b => a)(z: a)(xs: List[b]): a = xs match {
        case Nil => z
        case x :: xs => foldl(f)(f(z)(x))(!xs)
    }

    def foldl1[a](f: a => a => a)(xs: List[a]): a = xs match {
        case Nil => error("empty List")
        case x :: xs => foldl(f)(x)(!xs)
    }

    def foldr[a, b](f: a => &[b] => b)(z: b)(xs: List[a]): b = xs match {
        case Nil => z
        case x :: xs => f(x)(&(foldr(f)(z)(!xs)))
    }

    def foldr1[a](f: a => &[a] => a)(xs: List[a]): a = xs match {
        case Nil => error("empty List")
        case x !:: Nil => x
        case x :: xs => f(x)(&(foldr1(f)(!xs)))
    }

// Special folds
    def and(xs: List[Boolean]): Boolean = foldr(op_&&)(true)(xs)

    def or(xs: List[Boolean]): Boolean = foldr(op_||)(false)(xs)

    def any[a](p: a => Boolean)(xs: List[a]): Boolean = or(map(p)(xs))

    def all[a](p: a => Boolean)(xs: List[a]): Boolean = and(map(p)(xs))

    def sum[a](xs: List[a])(implicit i: Numeric[a]): a = foldl(curry(i.plus))(i.zero)(xs)

    def product[a](xs: List[a])(implicit i: Numeric[a]): a = foldl(curry(i.times))(i.one)(xs)

    def concat[a](xs: List[List[a]]): List[a] = foldr(List.appendr[a])(Nil)(xs)

    def concatMap[a, b](f: a => List[b])(xs: List[a]): List[b] = foldr[a, List[b]](x => y => f(x) ++ !y)(Nil)(xs)

    def maximum[a](xs: List[a])(implicit i: Ordering[a]): a = xs match {
        case Nil => error("empty List")
        case xs => foldl1(curry(i.max))(xs)
    }

    def minimum[a](xs: List[a])(implicit i: Ordering[a]): a = xs match {
        case Nil => error("empty List")
        case xs => foldl1(curry(i.min))(xs)
    }

// Building lists
    def scanl[a, b](f: a => b => a)(q: => a)(ls: List[b]): List[a] = { // why `q` is by-name?
        q :: (ls match {
            case Nil => Nil
            case x :: xs => scanl(f)(f(q)(x))(!xs)
        })
    }

    def scanl1[a](f: a => a => a)(xs: List[a]): List[a] = xs match {
        case Nil => error("empty List")
        case x :: xs => scanl(f)(x)(!xs)
    }

    def scanr[a, b](f: a => &[b] => b)(q0: b)(xs: List[a]): List[b] = xs match {
        case Nil => q0 :: Nil
        case x :: xs => {
            lazy val qs = scanr(f)(q0)(!xs)
            f(x)(&(head(qs))) :: qs
        }
    }

    def scanr1[a](f: a => &[a] => a)(xs: List[a]): List[a] = xs match {
        case Nil => Nil
        case x !:: Nil => x :: Nil
        case x :: xs => {
            lazy val qs = scanr1(f)(!xs)
            f(x)(&(head(qs))) :: qs
        }
    }

// Infinite lists
    def iterate[a](f: a => a)(x: a): List[a] = x :: iterate(f)(f(x))

    def repeat[a](x: a): List[a] = {
        lazy val xs: List[a] = x :: xs
        xs
    }

    def replicate[a](n: Int)(x: a): List[a] = take(n)(repeat(x))

    def cycle[a](xs: List[a]): List[a] = xs match {
        case Nil => error("empty List")
        case xs => {
            lazy val _xs: List[a] = xs ::: _xs
            _xs
        }
    }

// Sublists
    def take[a](n: Int)(xs: List[a]): List[a] = (n, xs) match {
        case (n, _) if n <= 0 => Nil
        case (_, Nil) => Nil
        case (n, x :: xs) => x :: take(n-1)(!xs)
    }

    @tailrec
    def drop[a](n: Int)(xs: List[a]): List[a] = (n, xs) match {
        case (_, Nil) => Nil
        case (n, _ :: xs) => drop(n-1)(!xs)
    }

    def splitAt[a](n: Int)(xs: List[a]): (List[a], List[a]) = (take(n)(xs), drop(n)(xs))

    def takeWhile[a](p: a => Boolean)(xs: List[a]): List[a] = xs match {
        case Nil => Nil
        case x :: xs => if (p(x)) x :: takeWhile(p)(!xs) else Nil
    }

    @tailrec
    def dropWhile[a](p: a => Boolean)(xs: List[a]): List[a] = xs match {
        case Nil => Nil
        case x :: _xs => if (p(x)) dropWhile(p)(!_xs) else xs
    }

    def span[a](p: a => Boolean)(xs: List[a]): (List[a], List[a]) = xs match {
        case Nil => (Nil, Nil)
        case x :: _xs => {
            if (p(x)) {
                val (ys, zs) = span(p)(!_xs)
                (x :: ys, zs)
            } else {
                (Nil, xs)
            }
        }
    }

    def break[a](p: a => Boolean)(xs: List[a]): (List[a], List[a]) = span[a](!p(_))(xs)

// Searching lists
    def elem[a](x: a)(xs: List[a]): Boolean = any[a](x == _)(xs)

    def notElem[a](x: a)(xs: List[a]): Boolean = all[a](x != _)(xs)

    @tailrec
    def lookup[a, b](key: a)(xs: List[(a, b)]): Option[b] = xs match {
        case Nil => None
        case (x, y) :: xys => if (key == x) Some(y) else lookup(key)(!xys)
    }

// Zipping and unzipping lists
    def zip[a, b](xs: List[a])(ys: List[b]): List[(a, b)] = (xs, ys) match {
        case (a :: as, b :: bs) => (a, b) :: zip(!as)(!bs)
        case _ => Nil
    }

    def zipWith[a, b, c](f: a => b => c)(xs: List[a])(ys: List[b]): List[c] = (xs, ys) match {
        case (a :: as, b :: bs) => f(a)(b) :: zipWith(f)(!as)(!bs)
        case _ => Nil
    }

    def unzip[a, b](xs: List[(a, b)]): (List[a], List[b]) = {
        foldr[(a, b), (List[a], List[b])](ab => abs => (ab._1 :: (!abs)._1, ab._2 :: (!abs)._2))((Nil, Nil))(xs)
    }

// Functions on strings


// Converting to String
    def show(x: Any): String = x.toString

// Basic I/O operations
    def putChar(x: Char): IO[Unit] = print(x)

    def putStr(x: String): IO[Unit] = print(x)

    def putStrLn(x: String): IO[Unit] = new IO[Unit] {
        override def unIO(): Unit = Predef.println(x)
    }

    def print(x: Any): IO[Unit] = new IO[Unit] {
        override def unIO(): Unit = Predef.print(x)
    }

    val getChar: IO[Char] = new IO[Char] {
        override def unIO(): Char = Predef.readChar()
    }

    val getLine: IO[String] = new IO[String] {
        override def unIO(): String = Predef.readLine()
    }
}
