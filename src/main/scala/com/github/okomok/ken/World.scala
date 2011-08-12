

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


// See: http://apocalisp.wordpress.com/2011/03/20/towards-an-effect-system-in-scala-part-1/


final class World {
    sealed class Tag

    object Tag {
        // ST[a] definition is enough to lookup StateT.asMonad.
        // implicit val STmonad = ST._monad
    }

    type ST[+a] = State[Tag, a]

    object ST extends Kind.quote1[ST] {
        def apply[a](r: => a): ST[a] = State { s => (r, s) }
        implicit val _asMonadState = State._asMonadState[Tag]
    }

    def runST[a](st: ST[a]): a = State.eval(st)(new Tag)

    sealed class STRef[a](private[World] var mutvar: a)

    def newSTRef[a](init: a): ST[STRef[a]] = ST { new STRef(init) }

    def readSTRef[a](ref: STRef[a]): ST[a] = ST { ref.mutvar }

    def writeSTRef[a](ref: STRef[a])(v: a): ST[Unit] = ST { ref.mutvar = v; () }

    def modifySTRef[a](ref: STRef[a])(f: a => a): ST[Unit] = {
        import ST._asMonadState.=<<
        (writeSTRef(ref)_ compose f) =<< readSTRef(ref)
    }
}
