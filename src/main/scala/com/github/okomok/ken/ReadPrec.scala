

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


final case class ReadPrec[+a](override val old: Int => ReadP[a]) extends NewtypeOf[Int => ReadP[a]]


object ReadPrec extends Newtype1[ReadPrec, ({type ot[+a] = Int => ReadP[a]})#ot] with MonadPlus[ReadPrec] with ThisIsInstance {
    // Overrides
    //
    // Newtype1
    private type nt[+a] = ReadPrec[a]
    private type ot[+a] = Prec => ReadP[a]
    override def newOf[a](ot: Lazy[ot[a]]): nt[a] = ReadPrec(ot)
    override def oldOf[a](nt: Lazy[nt[a]]): ot[a] = nt.run
    //
    // Functor
    private type f[+a] = ReadPrec[a]
    override def fmap[a, b](h: a => b): f[a] => f[b] = f => ReadPrec { n => ReadP.fmap(h)(f.get(n)) }
    // Monad
    private type m[+a] = ReadPrec[a]
    override def `return`[a](x: Lazy[a]): m[a] = ReadPrec { _ => ReadP.`return`(x) }
    override def op_>>=[a, b](f: m[a])(k: a => m[b]): m[b] = ReadPrec { n => for { a <- f.get(n) } { k(a).get(n) } }
    // MonadPlus
    override def mzero: m[Nothing] = pfail
    override def mplus[a](f1: m[a])(f2: Lazy[m[a]]): m[a] = f1 +++ f2

    // Precedences
    //
    type Prec = Int

    final val minPrec = 0

    // Operations
    //
    def lift[a](m: ReadP[a]): ReadPrec[a] = ReadPrec { _ => m }

    def step[a](f: ReadPrec[a]): ReadPrec[a] = ReadPrec { n => f.get(n+1) }

    def reset[a](f: ReadPrec[a]): ReadPrec[a] = ReadPrec { _ => f.get(minPrec) }

    def prec[a](n: Prec)(f: ReadPrec[a]): ReadPrec[a] = ReadPrec { c => if (c <= n) f.get(n) else ReadP.pfail }

    // Derived operations
    //
    val get: ReadPrec[Char] = lift(ReadP.get)

    val look: ReadPrec[String] = lift(ReadP.look)

    def op_+++[a](f1: ReadPrec[a])(f2: Lazy[ReadPrec[a]]): ReadPrec[a] = ReadPrec { n => ReadP.op_+++(f1.get(n))(f2.get(n)) }

    private[ken] sealed class Op_+++[a](f1: ReadPrec[a]) {
        def +++(f2: Lazy[ReadPrec[a]]): ReadPrec[a] = op_+++(f1)(f2)
    }
    implicit def +++[a](f1: ReadPrec[a]): Op_+++[a] = new Op_+++(f1)

    def op_<++[a](f1: ReadPrec[a])(f2: Lazy[ReadPrec[a]]): ReadPrec[a] = ReadPrec { n => ReadP.op_<++(f1.get(n))(f2.get(n)) }

    private[ken] sealed class Op_<++[a](f1: ReadPrec[a]) {
        def <++(f2: Lazy[ReadPrec[a]]): ReadPrec[a] = op_<++(f1)(f2)
    }
    implicit def <++[a](f1: ReadPrec[a]): Op_<++[a] = new Op_<++(f1)

    val pfail: ReadPrec[Nothing] = lift(ReadP.pfail)

    def choice[a](ps: List[ReadPrec[a]]): ReadPrec[a] = List.foldr(op_+++[a])(pfail)(ps)

    // Conversion between ReadPrec and ReadP
    //
    def readPrec_to_P[a](f: ReadPrec[a]): Int => ReadP[a] = n => f.get(n)

    def readP_to_Prec[a](f: Int => ReadP[a]): ReadPrec[a] = ReadPrec(f)

    def readPrec_to_S[a](f: ReadPrec[a]): Int => ReadS[a] = n => ReadP.readP_to_S(f.get(n))

    def readS_to_Prec[a](f: Int => ReadS[a]): ReadPrec[a] = ReadPrec { n => ReadP.readS_to_P(f(n)) }
}
