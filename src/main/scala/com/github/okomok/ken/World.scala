

// Copyright Shunsuke Sogame 2011.
//
// Copyright 2004, The University Court of the University of Glasgow.
// All rights reserved.
//
// Copyright (c) 2002 Simon Peyton Jones
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


import scala.collection.mutable.ArraySeq


class World {
    type This = this.type // makes other's `ST` incompatible.

    // ST
    //
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
        (writeSTRef(ref)_ `.` f) =<<: readSTRef(ref)
    }

    def atomicModifySTRef[a, b](r: STRef[a])(f: a => (a, b)): ST[b] = returnST {
        r.synchronized {
            val (new_r, b) = f(r.mutvar)
            r.mutvar = new_r
            b
        }
    }

    def unsafeIOToST[a](io: IO[a]): ST[a] = io match {
        case IO(m) => ST { s => m.asInstanceOf[This => (a, This)](s) }
    }

    def unsafeSTToIO[a](st: ST[a]): IO[a] = st match {
        case ST(m) => IO { s => m.asInstanceOf[IORep[a]](s) }
    }

    // STArray
    //
    final case class STArray[i, e](l: i, u: i, n: Int, rep: ArraySeq[e]) {
        override def equals(that: Any): Boolean = that match {
            case that: STArray[_, _] => rep.equals(that.rep)
            case _ => false
        }
        override def hashCode: Int = rep.hashCode
    }

    def newSTArray[i, e](rng: (i, i))(initial: e)(implicit ix: Ix[i]): ST[STArray[i, e]] = returnST { rng match {
        case (l, u) => {
            val n = ix.safeRangeSize(rng)
            val rep = ArraySeq.fill(n)(initial)
            STArray(l, u, n, rep)
        }
    } }

    def boundsSTArray[i, e](marr: STArray[i, e]): (i, i) = marr match {
        case STArray(l, u, _, _) => (l, u)
    }

    def numElementsSTArray[i, e](marr: STArray[i, e]): Int = marr match {
        case STArray(_, _, n, _) => n
    }

    def readSTArray[i, e](marr: STArray[i, e])(i: i)(implicit ix: Ix[i]): ST[e] = marr match {
        case STArray(l, u, n, _) => unsafeReadSTArray(marr)(ix.safeIndex(l, u)(n)(i))
    }

    def unsafeReadSTArray[i, e](marr: STArray[i, e])(i: Int): ST[e] = returnST { marr match {
        case STArray(_, _, _, rep) => rep(i)
    } }

    def writeSTArray[i, e](marr: STArray[i, e])(i: i)(e: e)(implicit ix: Ix[i]): ST[Unit] = marr match {
        case STArray(l, u, n, _) => unsafeWriteSTArray(marr)(ix.safeIndex(l, u)(n)(i))(e)
    }

    def unsafeWriteSTArray[i, e](marr: STArray[i, e])(i: Int)(e: e): ST[Unit] = returnST { marr match {
        case STArray(_, _, _, rep) => rep(i) = e
    } }
}
