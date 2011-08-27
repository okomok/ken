

// Copyright Shunsuke Sogame 2011.
//
// Copyright 2004, The University Court of the University of Glasgow.
// All rights reserved.
//
// Copyright (c) 2002 Simon Peyton Jones
//
// Distributed under the New BSD license.


// See: http://www.cse.chalmers.se/edu/course/afp/Papers/parser-claessen.pdf


package com.github.okomok
package ken


trait ReadP[+a] {
    def apply[b](k: a => ReadP.P[b]): ReadP.P[b]
}


object ReadP extends MonadPlus[ReadP] with Traversable[ReadP] with ThisIsInstance {
    // The P type
    //
    sealed abstract class P[+a] extends Up[P[a]] with Kind.constThis

    final case class Get[a](_1: Char => P[a]) extends P[a]
    final case class Look[a](_2: String_ => P[a]) extends P[a]
    final case object Fail extends P[Nothing]
    final case class Result[a](_1: a, _2: P[a]) extends P[a]
    final case class Final[a](_1: List[(a, String_)]) extends P[a] {
        Predef.require { not(List.`null`(_1)) }
    }

    object Get extends ReadP[Char]
    object Look extends ReadP[String_]

    object P extends MonadPlus[P] with Traversable[P] with ThisIsInstance {
        // Overrides
        //
        // Monad
        private type m[+a] = P[a]
        override def `return`[a](x: Lazy[a]): m[a] = Result(x, Fail)
        @deprecated("should not be used", "0.1.0")
        override def op_>>=[a, b](p: m[a])(k: a => m[b]): m[b] = p match {
            case Get(f) => Get(c => f(c) >>= k)
            case Look(f) => Look(s => f(s) >>= k)
            case Fail => Fail
            case Result(x, p) => k(x) _mplus_ (p >>= k)
            case Final(r) => `final` {
                for { (x, s) <- r; ys_ <- run(k(x))(s) } yield ys_
            }
        }
        // MonadPlus
        override def mzero: m[Nothing] = Fail
        override def mplus[a](p: m[a])(q: Lazy[m[a]]): m[a] = (p, q.!) match {
            // parallel processing
            case (Get(f1), Get(f2)) => Get(c => f1(c) _mplus_ f2(c))
            case (Result(x, p), q) => Result(x, p _mplus_ q)
            case (p, Result(x, q)) => Result(x, p _mplus_ q)
            case (Fail, p) => p
            case (p, Fail) => p
            case (Final(r), Final(t)) => Final(r ::: t)
            case (Final(r), Look(f)) => Look(s => Final(r ::: run(f(s))(s)))
            case (Final(r), p) => Look(s => Final(r ::: run(p)(s)))
            case (Look(f), Final(r)) => Look(s => Final(run(f(s))(s) ::: r))
            case (p, Final(r)) => Look(s => Final(run(p)(s) ::: r))
            case (Look(f), Look(g)) => Look(s => f(s) _mplus_ g(s))
            case (Look(f), p) => Look(s => f(s) _mplus_ p)
            case (p, Look(f)) => Look(s => p _mplus_ f(s))
        }

        // Operations
        //
        def `final`[a](r: List[(a, String_)]): P[a] = r match {
            case Nil => Fail
            case r => Final(r)
        }

        def run[a](p: P[a]): ReadS[a] = s => (p, s) match {
            case (Get(f), c :: s) => run(f(c))(s)
            case (Look(f), s) => run(f(s))(s)
            case (Result(x, p), s) => (x, s) :: run(p)(s)
            case (Final(r), _) => r
            case _ => Nil
        }
    }

    // ReadS
    //
    type ReadS[+a] = String_ => List[(a, String_)]

    // Overrides
    //
    // Functor
    private type f[+a] = ReadP[a]
    override def fmap[a, b](h: a => b)(f: f[a]): f[b] = new ReadP[b] {
        override def apply[c](k: b => P[c]): P[c] = f(k compose h)
    }
    // Monad
    private type m[+a] = ReadP[a]
    override def `return`[a](x: Lazy[a]): m[a] = new ReadP[a] {
        override def apply[b](k: a => P[b]) = k(x)
    }
    override def op_>>=[a, b](m: m[a])(f: a => m[b]): m[b] = new ReadP[b] {
        override def apply[c](k: b => P[c]): P[c] = m(a => f(a)(k))
    }
    // MonadPlus
    override def mzero: m[Nothing] = pfail
    override def mplus[a](f1: m[a])(f2: Lazy[m[a]]): m[a] = f1 +++ f2

    // Operations
    //
    val get: ReadP[Char] = Get

    val look: ReadP[String_] = Look

    def pfail[a]: ReadP[a] = new ReadP[a] {
        override def apply[b](k: a => P[b]): P[b] = Fail
    }

    def op_+++[a](f1: ReadP[a])(f2: ReadP[a]): ReadP[a] = new ReadP[a] {
        override def apply[b](k: a => P[b]): P[b] = f1(k) _mplus_ f2(k)
    }

    sealed class Op_+++[a](f1: ReadP[a]) {
        def +++(f2: ReadP[a]): ReadP[a] = op_+++(f1)(f2)
    }
    implicit def +++[a](f1: ReadP[a]): Op_+++[a] = new Op_+++(f1)
}
