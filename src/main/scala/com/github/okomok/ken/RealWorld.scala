

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


object RealWorld extends World {
    type IORef[a] = STRef[a]

    def newIORef[a](init: a): IO[IORef[a]] = stToIO(newSTRef(init))
    def readIORef[a](ref: IORef[a]): IO[a] = stToIO(readSTRef(ref))
    def writeIORef[a](ref: IORef[a])(v: a): IO[Unit] = stToIO(writeSTRef(ref)(v))
    def modifyIORef[a](ref: IORef[a])(f: a => a): IO[Unit] = stToIO(modifySTRef(ref)(f))
    def atomicModifyIORef[a, b](r: IORef[a])(f: a => (a, b)): IO[b] = stToIO(atomicModifySTRef(r)(f))

    // Coercions between IO and ST
    //
    def stToIO[a](st: ST[a]): IO[a] = st match {
        case ST(m) => IO(m)
    }

    def ioToST[a](io: IO[a]): ST[a] = io match {
        case IO(m) => ST(m)
    }
}
