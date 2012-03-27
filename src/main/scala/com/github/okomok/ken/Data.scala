

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


// @pending("inaccurate")
trait Data[a] extends Typeable[a] {
    final val asData: Data[apply0] = this

    // Core
    //
    def gfoldl[c[_]](k: GenericL[c])(z: Pure[c])(a: a): c[a] = z(a)

    // How?
    // def gunfold...

    type gmapT = GenericT => a => a
    def gmapT: gmapT = f => x0 => {
        type c[d] = d
        val k = new GenericL[c] {
            override def apply[d, b](c: c[d => b])(x: d)(implicit ii: Data[d]): c[b] = c(f(x))
        }
        val z = new Pure[c] {
            override def apply[g](g: g): c[g] = g
        }
        gfoldl[c](k)(z)(x0)
    }

    def gmapQl[r, r_](o: r => r_ => r)(r: r)(f: GenericQ[r_])(x0: a): r = {
        type c[_] = r
        val k = new GenericL[c] {
            override def apply[d, b](c: c[d => b])(x: d)(implicit ii: Data[d]): c[b] = {
                o(c)(f(x))
            }
        }
        val z = new Pure[c] {
            override def apply[g](g: g): c[g] = r
        }
        gfoldl(k)(z)(x0)
    }

    def gmapQr[r, r_](o: r_ => r => r)(r0: r)(f: GenericQ[r_])(x0: a): r = {
        type c[_] = r => r
        val k = new GenericL[c] {
            override def apply[d, b](c: c[d => b])(x: d)(implicit ii: Data[d]): c[b] = { r =>
                c(o(f(x))(r))
            }
        }
        val z = new Pure[c] {
            override def apply[g](g: g): c[g] = id[r]
        }
        gfoldl(k)(z)(x0)(r0)
    }

    def gmapQi[u](i: Int)(f: GenericQ[u])(x: a): u = {
        type c[_] = (Int, Maybe[u])
        val k = new GenericL[c] {
            override def apply[d, b](c: c[d => b])(a: d)(implicit ii: Data[d]): c[b] = c match {
                case (i_, q) => (i_ + 1, if (i === i_) Just(f(a)) else q)
            }
        }
        val z = new Pure[c] {
            override def apply[g](g: g): c[g] = (0, Nothing)
        }

        gfoldl(k)(z)(x) match {
            case (_, q) => Maybe.fromJust(q)
        }
    }

    def gmapQ[u](f: GenericQ[u])(x0: a): List[u] = gmapQr(List.op_::[u])(Nil.of[u])(f)(x0)

    def gmapM[m[+_]](f: GenericM[m])(x0: a)(implicit j: Monad[m]): m[a] = {
        type c[d] = m[d]
        val k = new GenericL[c] {
            override def apply[d, b](c: c[d => b])(x: d)(implicit ii: Data[d]): c[b] = {
                import j.`for`
                for {
                    c_ <- c
                    x_ <- f(x)
                } yield c_(x_)
            }
        }
        val z = new Pure[c] {
            override def apply[g](g: g): c[g] = j.`return`(g)
        }
        gfoldl(k)(z)(x0)
    }

    def gmapMp[m[+_]](f: GenericM[m])(x: a)(implicit j: MonadPlus[m]): m[a] = {
        type c[d] = m[(d, Bool)]
        import j.{>>=, _mplus_}
        val k = new GenericL[c] {
            override def apply[d, b](c: c[d => b])(y: d)(implicit ii: Data[d]): c[b] = {
                c >>= { case (h, b) =>
                    (f(y) >>= { y_ => j.`return`(h(y_), True) }) _mplus_ j.`return`(h(y), b)
                }
            }
        }
        val z = new Pure[c] {
            override def apply[g](g: g): c[g] = j.`return`(g, False)
        }
        gfoldl[c](k)(z)(x) >>= { case (x_, b) =>
            if (b) j.`return`(x_) else j.mzero
        }
    }

    def gmapMo[m[+_]](f: GenericM[m])(x: a)(implicit j: MonadPlus[m]): m[a] = {
        type c[d] = m[(d, Bool)]
        import j.{>>=, _mplus_}
        val k = new GenericL[c] {
            override def apply[d, b](c: c[d => b])(y: d)(implicit ii: Data[d]): c[b] = {
                c >>= { case (h, b) =>
                    if (b) j.`return`(h(y), b)
                    else (f(y) >>= { y_ => j.`return`(h(y_), True) }) _mplus_ j.`return`(h(y), b)
                }
            }
        }
        val z = new Pure[c] {
            override def apply[g](g: g): c[g] = j.`return`(g, False)
        }
        gfoldl[c](k)(z)(x) >>= { case (x_, b) =>
            if (b) j.`return`(x_) else j.mzero
        }
    }
}


trait DataProxy[a] extends Data[a] with TypeableProxy[a] {
    type selfData = Data[a]
    def selfData: selfData
    override def selfTypeable: selfTypeable = selfData

    override def gfoldl[c[_]](k: GenericL[c])(z: Pure[c])(a: a): c[a] = selfData.gfoldl(k)(z)(a)
    override def gmapT: gmapT = selfData.gmapT
    override def gmapQl[r, r_](o: r => r_ => r)(r: r)(f: GenericQ[r_])(x0: a): r = selfData.gmapQl(o)(r)(f)(x0)
    override def gmapQr[r, r_](o: r_ => r => r)(r0: r)(f: GenericQ[r_])(x0: a): r = selfData.gmapQr(o)(r0)(f)(x0)
    override def gmapQi[u](i: Int)(f: GenericQ[u])(x: a): u = selfData.gmapQi(i)(f)(x)
    override def gmapQ[u](f: GenericQ[u])(x0: a): List[u] = selfData.gmapQ(f)(x0)
    override def gmapM[m[+_]](f: GenericM[m])(x0: a)(implicit j: Monad[m]): m[a] = selfData.gmapM(f)(x0)(j)
    override def gmapMp[m[+_]](f: GenericM[m])(x: a)(implicit j: MonadPlus[m]): m[a] = selfData.gmapMp(f)(x)(j)
    override def gmapMo[m[+_]](f: GenericM[m])(x: a)(implicit j: MonadPlus[m]): m[a] = selfData.gmapMo(f)(x)(j)
}


object Data extends DataInstance with DataShortcut {
    def apply[a <: Kind.Function0](implicit i: Data[a#apply0]): Data[a#apply0] = i
}


sealed trait DataInstance { this: Data.type =>
    implicit def ofDefault[a](implicit t: Typeable[a]): Data[a] = new Data[a] with TypeableProxy[a] {
        override def selfTypeable: selfTypeable = t
    }

    implicit def _ofList[v](implicit i: Data[v], t: Typeable[List[v]]): Data[List[v]] = new Data[List[v]] with TypeableProxy[List[v]] {
        private type a = List[v]
        override def selfTypeable: selfTypeable = t
        override def gfoldl[c[_]](f: GenericL[c])(z: Pure[c])(a: a): c[a] = a match {
            case Nil => z(Nil)
            case x :: xs => f( f(z( Function.from(List.op_!::[v]) ))(x) )(xs.!)(this)
        }
    }

    implicit def _ofMaybe[v](implicit i: Data[v], t: Typeable[Maybe[v]]): Data[Maybe[v]] = new Data[Maybe[v]] with TypeableProxy[Maybe[v]] {
        private type a = Maybe[v]
        override def selfTypeable: selfTypeable = t
        override def gfoldl[c[_]](f: GenericL[c])(z: Pure[c])(a: a): c[a] = a match {
            case Nothing => z(Nothing)
            case Just(x) => f(z(Just(_: v).up))(x)
        }
    }

    implicit val _ofOrdering: Data[Ordering] = new Data[Ordering] with TypeableProxy[Ordering] {
        private type a = Ordering
        override def selfTypeable: selfTypeable = Typeable.of[Ordering]
        override def gfoldl[c[_]](f: GenericL[c])(z: Pure[c])(a: a): c[a] = a match {
            case LT => z(LT)
            case EQ => z(EQ)
            case GT => z(GT)
        }
    }

    implicit def _ofEither[v, w](implicit i: Data[v], j: Data[w], t: Typeable[Either[v, w]]): Data[Either[v, w]] = new Data[Either[v, w]] with TypeableProxy[Either[v, w]] {
        private type a = Either[v, w]
        override def selfTypeable: selfTypeable = t
        override def gfoldl[c[_]](f: GenericL[c])(z: Pure[c])(a: a): c[a] = a match {
            case Left(a) => f(z(Left(_: v).of[v, w]))(a)
            case Right(a) => f(z(Right(_: w).of[v, w]))(a)
        }
    }

    // Tuples
    //
    implicit def _ofTuple2[v1, v2](implicit i1: Data[v1], i2: Data[v2], t: Typeable[Tuple2[v1, v2]]): Data[Tuple2[v1, v2]] = new Data[Tuple2[v1, v2]] with TypeableProxy[Tuple2[v1, v2]] {
        private type a = Tuple2[v1, v2]
        override def selfTypeable: selfTypeable = t
        override def gfoldl[c[_]](f: GenericL[c])(z: Pure[c])(a: a): c[a] = a match {
            case (w1, w2) => f( f(z((v1: v1) => (v2: v2) => (v1, v2)))(w1) )(w2)
        }
    }

    implicit def _ofTuple3[v1, v2, v3](implicit i1: Data[v1], i2: Data[v2], i3: Data[v3], t: Typeable[Tuple3[v1, v2, v3]]): Data[Tuple3[v1, v2, v3]] = new Data[Tuple3[v1, v2, v3]] with TypeableProxy[Tuple3[v1, v2, v3]] {
        private type a = Tuple3[v1, v2, v3]
        override def selfTypeable: selfTypeable = t
        override def gfoldl[c[_]](f: GenericL[c])(z: Pure[c])(a: a): c[a] = a match {
            case (w1, w2, w3) => f( f( f(z((v1: v1) => (v2: v2) => (v3: v3) => (v1, v2, v3)))(w1) )(w2) )(w3)
        }
    }
}


trait DataShortcut extends TypeableShortcut {
    def gfoldl[a, c[_]](k: GenericL[c])(z: Pure[c])(a: a)(implicit i: Data[a]): c[a] = i.gfoldl(k)(z)(a)
    def gmapT[a](f: GenericT)(x: a)(implicit i: Data[a]): a = i.gmapT(f)(x)
    def gmapQl[a, r, r_](o: r => r_ => r)(r: r)(f: GenericQ[r_])(x0: a)(implicit i: Data[a]): r = i.gmapQl(o)(r)(f)(x0)
    def gmapQr[a, r, r_](o: r_ => r => r)(r0: r)(f: GenericQ[r_])(x0: a)(implicit i: Data[a]): r = i.gmapQr(o)(r0)(f)(x0)
    def gmapQi[a, u](n: Int)(f: GenericQ[u])(x: a)(implicit i: Data[a]): u = i.gmapQi(n)(f)(x)
    def gmapQ[a, u](f: GenericQ[u])(x0: a)(implicit i: Data[a]): List[u] = i.gmapQ(f)(x0)
    def gmapM[a, m[+_]](f: GenericM[m])(x0: a)(implicit i: Data[a], j: Monad[m]): m[a] = i.gmapM(f)(x0)(j)
    def gmapMp[a, m[+_]](f: GenericM[m])(x: a)(implicit i: Data[a], j: MonadPlus[m]): m[a] = i.gmapMp(f)(x)(j)
    def gmapMo[a, m[+_]](f: GenericM[m])(x: a)(implicit i: Data[a], j: MonadPlus[m]): m[a] = i.gmapMo(f)(x)(j)
}
