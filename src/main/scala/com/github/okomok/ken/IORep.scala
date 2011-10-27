

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


object IORep {
    def done[a](a: a, s: RealWorld.type): Product2[a, RealWorld.type] with Trampoline[a] = new Trampoline.Done(a) with Product2[a, RealWorld.type] {
        override val _1 = a
        override val _2 = s
    }

    def cont[z, a](rep: IORep[z], k: z => IORep[a], s: RealWorld.type): Product2[a, RealWorld.type] with Trampoline[a] = new Trampoline.Cont(rep(s), (z: z) => k(z)(s)) with Product2[a, RealWorld.type] {
        override lazy val _1 = _eval
        override val _2 = s
    }
}
