

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait For[m[+_], +a] {
    def map[b](f: a => b): m[b] = error("shall be a Functor")
    def flatMap[b](k: a => m[b]): m[b] = error("shall be a Monad")
    final def foreach[b](k: a => m[b]): m[b] = flatMap(k)
    def filter(p: a => Bool): m[a] = map(a => a.ensuring(p, "shall be a MonadPlus"))
    final def withFilter(p: a => Bool): m[a] = filter(p)
}


trait ForProxy[m[+_], +a] extends For[m, a] {
    def selfFor: For[m, a]

    override def map[b](f: a => b): m[b] = selfFor.map(f)
    override def flatMap[b](k: a => m[b]): m[b] = selfFor.flatMap(k)
    override def filter(p: a => Bool): m[a] = selfFor.filter(p)
}
