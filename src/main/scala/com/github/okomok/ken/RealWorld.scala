

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


object RealWorld extends World {

    // Coercions between IO and ST
    //
    def stToIO[a](st: ST[a]): IO[a] = st match {
        case ST(m) => IO(m)
    }

    def ioToST[a](io: IO[a]): ST[a] = io match {
        case IO(m) => ST(m)
    }

    // IORef
    //
    type IORef[a] = STRef[a]

    def newIORef[a](init: a): IO[IORef[a]] = stToIO(newSTRef(init))
    def readIORef[a](ref: IORef[a]): IO[a] = stToIO(readSTRef(ref))
    def writeIORef[a](ref: IORef[a])(v: a): IO[Unit] = stToIO(writeSTRef(ref)(v))
    def modifyIORef[a](ref: IORef[a])(f: a => a): IO[Unit] = stToIO(modifySTRef(ref)(f))
    def atomicModifyIORef[a, b](r: IORef[a])(f: a => (a, b)): IO[b] = stToIO(atomicModifySTRef(r)(f))

    // IOArray
    //
    type IOArray[i, e] = STArray[i, e]

    def newIOArray[i, e](rng: (i, i))(initial: e)(implicit ix: Ix[i]): IO[IOArray[i, e]] = stToIO(newSTArray(rng)(initial)(ix))
    def boundsIOArray[i, e](marr: IOArray[i, e]): (i, i) = boundsSTArray(marr)
    def numElementsIOArray[i, e](marr: IOArray[i, e]): Int = numElementsSTArray(marr)
    def readIOArray[i, e](marr: IOArray[i, e])(i: i)(implicit ix: Ix[i]): IO[e] = stToIO(readSTArray(marr)(i)(ix))
    def unsafeReadIOArray[i, e](marr: STArray[i, e])(i: Int): IO[e] = stToIO(unsafeReadSTArray(marr)(i))
    def writeIOArray[i, e](marr: STArray[i, e])(i: i)(e: e)(implicit ix: Ix[i]): IO[Unit] = stToIO(writeSTArray(marr)(i)(e)(ix))
    def unsafeWriteIOArray[i, e](marr: STArray[i, e])(i: Int)(e: e): IO[Unit] = stToIO(unsafeWriteSTArray(marr)(i)(e))
}
