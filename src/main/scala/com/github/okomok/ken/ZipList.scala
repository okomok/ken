

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


final case class ZipList[+a](override val get: List[a]) extends NewtypeOf[List[a]]


object ZipList extends Newtype1[ZipList, List] with Applicative[ZipList] with ThisIsInstance {
    def getZipList[a](m: ZipList[a]): List[a] = m.run

    // Overrides
    //
    // Newtype1
    private[this] type nt[+a] = ZipList[a]
    private[this] type ot[+a] = List[a]
    override def newOf[a](ot: Lazy[ot[a]]): nt[a] = ZipList(ot)
    override def oldOf[a](nt: Lazy[nt[a]]): ot[a] = nt.run
    // Functor
    private[this] type f[+a] = ZipList[a]
    override def fmap[a, b](f: a => b)(xs: f[a]): f[b] = ZipList { List.map(f)(run(xs)) }
    // Applicative
    private[this] type m[+a] = f[a]
    override def pure[a](x: Lazy[a]): f[a] = ZipList { List.repeat(x) }
    override def op_<*>[a, b](fs: f[a => b])(xs: f[a]): f[b] = ZipList { List.zipWith[a => b, a, b](id)(run(fs))(run(xs)) }
}
