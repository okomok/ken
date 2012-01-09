

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


// type IORep[+a] = RealWorld.type => Product2[a, RealWorld.type] with Trampoline[a]
// val IORep = _IORep

// BTW, `type IORep[+a] = RealWorld.type => Trampoline[(a, RealWorld.type)]` is also ok,
// but I want to make the signature as similar to Haskell's one as possible.


private[ken] object _IORep {

    private[ken] def done[a](a: a, s: RealWorld.type): Product2[a, RealWorld.type] with Trampoline[a] = new Trampoline(Trampoline.Done(a)) with Product2[a, RealWorld.type] {
        override val _1: a = a
        override val _2: RealWorld.type = s
        override def canEqual(that: Any): Boolean = that match {
            case that: Product2[_, _] => true
            case _ => false
        }
    }

    private[ken] def cont[z, a](rep: IORep[z], k: z => IORep[a], s: RealWorld.type): Product2[a, RealWorld.type] with Trampoline[a] = new Trampoline(Trampoline.Cont(rep(s), (z: z) => k(z)(s))) with Product2[a, RealWorld.type] {
        override lazy val _1: a = _eval
        override val _2: RealWorld.type = s
        override def canEqual(that: Any): Boolean = that match {
            case that: Product2[_, _] => true
            case _ => false
        }
    }
}
