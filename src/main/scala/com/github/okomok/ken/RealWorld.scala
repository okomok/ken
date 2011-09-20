

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

    object IORef {
        def `new`[a](init: a): IO[IORef[a]] = stToIO(STRef.`new`(init))
        def read[a](ref: IORef[a]): IO[a] = stToIO(STRef.read(ref))
        def write[a](ref: IORef[a])(v: a): IO[Unit] = stToIO(STRef.write(ref)(v))
        def modify[a](ref: IORef[a])(f: a => a): IO[Unit] = stToIO(STRef.modify(ref)(f))
        def atomicModify[a, b](r: IORef[a])(f: a => (a, b)): IO[b] = stToIO(STRef.atomicModify(r)(f))
    }

    // IOArray
    //
    type IOArray[i, e] = STArray[i, e]

    object IOArray {
        def `new`[i, e](rng: (i, i))(initial: e)(implicit ix: Ix[i]): IO[IOArray[i, e]] = stToIO(STArray.`new`(rng)(initial)(ix))
        def bounds[i, e](marr: IOArray[i, e]): (i, i) = STArray.bounds(marr)
        def numElements[i, e](marr: IOArray[i, e]): Int = STArray.numElements(marr)
        def read[i, e](marr: IOArray[i, e])(i: i)(implicit ix: Ix[i]): IO[e] = stToIO(STArray.read(marr)(i)(ix))
        def unsafeRead[i, e](marr: STArray[i, e])(i: Int): IO[e] = stToIO(STArray.unsafeRead(marr)(i))
        def writeIOArray[i, e](marr: STArray[i, e])(i: i)(e: e)(implicit ix: Ix[i]): IO[Unit] = stToIO(STArray.write(marr)(i)(e)(ix))
        def unsafeWrite[i, e](marr: STArray[i, e])(i: Int)(e: e): IO[Unit] = stToIO(STArray.unsafeWrite(marr)(i)(e))
    }
}
