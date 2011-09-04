

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


class World {
    type This = this.type

    type ST[+a] = State[This, a]

    object ST extends MonadStateProxy[This, ST] {
        override val selfMonadState = instance[MonadState[This, ST]]

        def apply[a](run: This => (a, This)): ST[a] = State(run)
        def unapply[a](st: ST[a]): Option[This => (a, This)] = Some(st.run)
    }

    private[this] def returnST[a](x: => a): ST[a] = State { s => (x, s) }

    def runST[a](st: ST[a]): a = State.eval(st)(this)

    sealed class STRef[a](private[World] var mutvar: a)

    def newSTRef[a](init: a): ST[STRef[a]] = returnST { new STRef(init) }

    def readSTRef[a](ref: STRef[a]): ST[a] = returnST { ref.mutvar }

    def writeSTRef[a](ref: STRef[a])(v: a): ST[Unit] = returnST { ref.mutvar = v; () }

    def modifySTRef[a](ref: STRef[a])(f: a => a): ST[Unit] = {
        import ST.=<<:
        (writeSTRef(ref)_ compose f) =<<: readSTRef(ref)
    }

    def unsafeIOToST[a](io: IO[a]): ST[a] = io match {
        case IO(m) => ST { s => m.asInstanceOf[This => (a, This)](s) }
    }

    def unsafeSTToIO[a](st: ST[a]): IO[a] = st match {
        case ST(m) => IO { s => m.asInstanceOf[RealWorld.type => (a, RealWorld.type)](s) }
    }
}
