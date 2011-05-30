

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


import scala.annotation.tailrec


object Prelude {

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
    import List.{Nil, ::, #:: }

    def map[a, b](f: a => b)(xs: List[a]): List[b] = xs match {
        case Nil => Nil
        case x :: xs => f(x) :: map(f)(xs())
    }

    def op_++[a](xs: List[a])(ys: List[a]): List[a] = xs match {
        case Nil => ys
        case x :: xs => x :: (xs() ++ ys)
    }

    def filter[a](pred: a => Boolean)(xs: List[a]): List[a] = xs match {
        case Nil => Nil
        case x :: xs => {
            if (pred(x)) {
                x :: filter(pred)(xs())
            } else {
                filter(pred)(xs())
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
        case x #:: Nil => x
        case _ :: xs => last(xs())
    }

    def tail[a](xs: List[a]): List[a] = xs match {
        case Nil => error("empty List")
        case _ :: xs => xs()
    }

    def init[a](xs: List[a]): List[a] = xs match {
        case Nil => error("empty List")
        case x #:: Nil => Nil
        case x :: xs => x :: init(xs())
    }

    def `null`(xs: List[_]): Boolean = xs match {
        case Nil => true
        case _ => false
    }

    def length(xs: List[_]): Int = undefined

    def op_!![a](xs: List[a])(n: Int): a = (xs, n) match {
        case (_, n) if n < 0 => error("negative index")
        case (Nil, _) => error("index too large")
        case (x :: _, 0) => x
        case (_ :: xs, n) => xs() !! (n-1)
    }

    def reverse[a](xs: List[a]): List[a] = undefined

// Reducing lists (folds)
    @tailrec
    def foldl[a, b](f: a => b => a)(z: a)(xs: List[b]): a = xs match {
        case Nil => z
        case x :: xs => foldl(f)(f(z)(x))(xs())
    }

    def foldl1[a](f: a => a => a)(xs: List[a]): a = xs match {
        case Nil => error("empty List")
        case x :: xs => foldl(f)(x)(xs())
    }

    def foldr[a, b](f: a => Lazy[b] => b)(z: b)(xs: List[a]): b = xs match {
        case Nil => z
        case x :: xs => f(x)(foldr(f)(z)(xs()))
    }

    def foldr1[a](f: a => Lazy[a] => a)(xs: List[a]): a = xs match {
        case Nil => error("empty List")
        case x #:: Nil => x
        case x :: xs => f(x)(foldr1(f)(xs()))
    }

// Special folds
    def concat[a](xs: List[List[a]]): List[a] = foldr[List[a], List[a]](a1 => a2 => a1 ::: a2())(Nil)(xs)


// Building lists


// Infinite lists


// Sublists


// Searching lists


// Zipping and unzipping lists


// Functions on strings


// Converting to String
    def show(x: Any): String = x.toString

// Basic IO
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

    @tailrec
    def op_==(xs: List[_])(ys: List[_]): Boolean = (xs, ys) match {
        case (Nil, Nil) => true
        case (Nil, _) => false
        case (_, Nil) => false
        case (x :: xs, y :: ys) => {
            if (x == y) op_==(xs())(ys()) else false
        }
    }
}
