

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


class World { world =>
    type This = this.type // makes other's `ST` incompatible.

    // ST
    //
    type ST[+a] = State[This, a]

    object ST extends MonadStateProxy[This, ST] {
        override val selfMonadState = instance[MonadState[This, ST]]

        def apply[a](run: This => (a, This)): ST[a] = State(run)
        def unapply[a](st: ST[a]): Option[This => (a, This)] = Some(st.run)

        def run[a](st: ST[a]): a = State.eval(st)(world)
    }

    private[this] def returnST[a](x: => a): ST[a] = State { s => (x, s) }

    // Coercions between IO and ST
    //
    def unsafeIOToST[a](io: IO[a]): ST[a] = io match {
        case IO(m) => ST { s =>
            m(s.asInstanceOf[RealWorld.type]) match {
                case Product2(a, _) => (a, s)
            }
        }
    }

    def unsafeSTToIO[a](st: ST[a]): IO[a] = st match {
        case ST(m) => IO { s =>
            m(s.asInstanceOf[This]) match {
                case (a, _) => IORep.Done(a, s)
            }
        }
    }

    // STRef
    //
    sealed class STRef[a](private[World] var mutvar: a)

    object STRef {
        def `new`[a](init: a): ST[STRef[a]] = returnST { new STRef(init) }

        def read[a](ref: STRef[a]): ST[a] = returnST { ref.mutvar }

        def write[a](ref: STRef[a])(v: a): ST[Unit] = returnST { ref.mutvar = v; () }

        def modify[a](ref: STRef[a])(f: a => a): ST[Unit] = {
            import ST.=<<:
            (write(ref)_ `.` f) =<<: read(ref)
        }

        def atomicModify[a, b](r: STRef[a])(f: a => (a, b)): ST[b] = returnST {
            r.synchronized {
                val (new_r, b) = f(r.mutvar)
                r.mutvar = new_r
                b
            }
        }
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

    object STArray {
        def `new`[i, e](rng: (i, i))(initial: e)(implicit ix: Ix[i]): ST[STArray[i, e]] = returnST { rng match {
            case (l, u) => {
                val n = ix.safeRangeSize(rng)
                val rep = ArraySeq.fill(n)(initial)
                STArray(l, u, n, rep)
            }
        } }

        def bounds[i, e](marr: STArray[i, e]): (i, i) = marr match {
            case STArray(l, u, _, _) => (l, u)
        }

        def numElements[i, e](marr: STArray[i, e]): Int = marr match {
            case STArray(_, _, n, _) => n
        }

        def read[i, e](marr: STArray[i, e])(i: i)(implicit ix: Ix[i]): ST[e] = marr match {
            case STArray(l, u, n, _) => unsafeRead(marr)(ix.safeIndex(l, u)(n)(i))
        }

        def unsafeRead[i, e](marr: STArray[i, e])(i: Int): ST[e] = returnST { marr match {
            case STArray(_, _, _, rep) => rep(i)
        } }

        def write[i, e](marr: STArray[i, e])(i: i)(e: e)(implicit ix: Ix[i]): ST[Unit] = marr match {
            case STArray(l, u, n, _) => unsafeWrite(marr)(ix.safeIndex(l, u)(n)(i))(e)
        }

        def unsafeWrite[i, e](marr: STArray[i, e])(i: Int)(e: e): ST[Unit] = returnST { marr match {
            case STArray(_, _, _, rep) => rep(i) = e
        } }
    }
}
