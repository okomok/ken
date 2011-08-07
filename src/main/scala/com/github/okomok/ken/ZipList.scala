

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


final case class ZipList[+a](override val get: List[a]) extends Strong[List[a]]


object ZipList extends Kind.Strong1 with Applicative[ZipList] with ThisIsInstance {
    override type weak[+a] = List[a]

    def run[a](m: ZipList[a]): List[a] = m.run
    def getZipList[a](m: ZipList[a]): List[a] = m.run

    // Overrides
    //
    // Functor
    private[this] type f[+a] = ZipList[a]
    override def fmap[a, b](f: a => b)(xs: f[a]): f[b] = ZipList { List.map(f)(run(xs)) }
    // Applicative
    private[this] type m[+a] = f[a]
    override def pure[a](x: => a): f[a] = ZipList { List.repeat(x) }
    override def op_<*>[a, b](fs: f[a => b])(xs: f[a]): f[b] = ZipList { List.zipWith[a => b, a, b](id)(run(fs))(run(xs)) }

    // Instances
    //
    implicit val weak: Imply1[ZipList, List] = new Imply1[ZipList, List] {
        private[this] type p[+a] = ZipList[a]
        private[this] type d[+a] = List[a]
        override def imply[a](p: p[a]): d[a] = run(p)
        override def unimply[a](d: => d[a]): p[a] = ZipList { d }
    }
}
