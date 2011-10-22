

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2000-2006, Koen Claessen
// Copyright (c) 2006, Bjorn Bringert
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package quickcheck


final case class Rose[+a](x: a, ts: Lazy[List[Rose[a]]]) {
    override def toString: JString = "Rose(" + x + "," + ts.! + ")"
}


object Rose extends Monad[Rose] with ThisIsInstance {
    // Overrides
    //
    // Functor
    private type f[+a] = Rose[a]
    override def fmap[a, b](f: a => b): f[a] => f[b] = {
        case Rose(x, rs) => Rose(f(x), for { r <- rs.! } yield fmap(f)(r))
    }
    // Monad
    private type m[+a] = Rose[a]
    override def `return`[a](x: Lazy[a]): m[a] = Rose(x, Nil)
    override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = join(fmap(k)(m))

    override def join[a](rs: Rose[Rose[a]]): Rose[a] = rs match {
        case Rose(Rose(x, ts), tts) => Rose(x, List.map((y: Rose[Rose[a]]) => join(y))(tts.!) ++: ts.!)
    }
}
