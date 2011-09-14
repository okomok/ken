

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2000-2006, Koen Claessen
// Copyright (c) 2006, Bjorn Bringert
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package quickcheck


import Random.StdGen


final case class Gen[+a](override val get: StdGen => Int => a) extends Strong[StdGen => Int => a]


object Gen extends Monad[Gen] with ThisIsInstance {
    // Overrides
    //
    // Functor
    private type f[+a] = Gen[a]
    override def fmap[a, b](f: a => b)(g: f[a]): f[b] = Gen(r => n => f(g.get(r)(n)))
    // Monad
    private type m[+a] = Gen[a]
    override def `return`[a](x: Lazy[a]): m[a] = Gen(_ => _ => x)
    override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = Gen { r => n => {
        val (r1, r2) = StdGen.split(r)
        val Gen(m_) = k(m.get(r1)(n))
        m_(r2)(n)
    } }

    def variant[n, a](k: n)(g: Gen[a])(implicit i: Integral[n]): Gen[a] = {
        import i._
        val k_ = k _div_ 2
        lazy val var_ : n => StdGen => StdGen = k => {
            (if (k == k_) id[StdGen] else var_(k_)) `.` (if (even(k)) Pair.fst[StdGen]_ else Pair.snd[StdGen]_) `.` StdGen.split
        }
        Gen(r => n => g.get(var_(k)(r))(n))
    }

    def sized[a](f: Int => Gen[a]): Gen[a] = Gen(r => n => f(n).get(r)(n))

    def resize[a](n: Int)(g: Gen[a]): Gen[a] = Gen(r => _ => g.get(r)(n))


}
