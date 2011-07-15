

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


// See: http://apocalisp.wordpress.com/2011/03/20/towards-an-effect-system-in-scala-part-1/


final class STs {
    sealed class World

    object World {
        implicit val STmonad = ST.monad
    }

    type ST[+a] = State[World, a]

    object ST {
        def apply[a](r: => a): ST[a] = State { s => (r, s) }
        implicit val monad = State.monad[World]
    }

    def runST[a](st: ST[a]): a = State.eval(st)(new World)

    sealed class STRef[a](private[STs] var mutvar: a)

    def newSTRef[a](init: a): ST[STRef[a]] = ST { new STRef(init) }

    def readSTRef[a](ref: STRef[a]): ST[a] = ST { ref.mutvar }

    def writeSTRef[a](ref: STRef[a])(v: a): ST[Unit] = ST { ref.mutvar = v; () }

    def modifySTRef[a](ref: STRef[a])(f: a => a): ST[Unit] = {
        import ST.monad.=<<
        (writeSTRef(ref)_ compose f) =<< readSTRef(ref)
    }
}
