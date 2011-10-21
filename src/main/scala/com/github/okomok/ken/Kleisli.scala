

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


final case class Kleisli[m[+_], -a, +b](override val old: a => m[b]) extends NewtypeOf[a => m[b]]


object Kleisli extends KleisliOp with KleisliAs with Kind.FunctionLike {
    trait apply[m[+_]] extends apply1[m]
    trait apply1[m[+_]] extends Kind.Newtype2 {
        override type apply2[-a, +b] = Kleisli[m, a, b]
        override type oldtype2[-a, +b] = a => m[b]
    }
}


private[ken] sealed trait KleisliOp { this: Kleisli.type =>
    def run[m[+_], a, b](f: Kleisli[m, a, b]): a => m[b] = f.run
}


private[ken] sealed trait KleisliAs0 { this: Kleisli.type =>
/*
    implicit def _asNewtype2[m[+_]]: Newtype2[apply1[m]#apply2, ({type ot[-a, +b] = a => m[b]})#ot] = new Newtype2[apply1[m]#apply2, ({type ot[-a, +b] = a => m[b]})#ot] {
        private type nt[-a, +b] = Kleisli[m, a, b]
        private type ot[-a, +b] = a => m[b]
        override def newOf[a, b](ot: Lazy[ot[a, b]]): nt[a, b] = Kleisli(ot)
        override def oldOf[a, b](nt: Lazy[nt[a, b]]): ot[a, b] = nt.run
    }
*/
    implicit def _asArrowPlus[m[+_]](implicit i: MonadPlus[m]): ArrowPlus[apply1[m]#apply2] = new ArrowPlus[apply1[m]#apply2] with ArrowProxy[apply1[m]#apply2] {
        private type a[-a, +b] = Kleisli[m, a, b]
        override val selfArrow = _asArrow(i)
        override def zeroArrow[b, c]: a[b, c] = Kleisli { _ => i.mzero }
        override def op_<+>:[b, c](f: a[b, c])(g: Lazy[a[b, c]]): a[b, c] = Kleisli { x =>
            import i._mplus_
            (f.run)(x) _mplus_ g.run(x)
        }
    }

    implicit def _asArrowChoice[m[+_]](implicit i: Monad[m]): ArrowChoice[apply1[m]#apply2] = new ArrowChoice[apply1[m]#apply2] with ArrowProxy[apply1[m]#apply2] {
        private type a[-a, +b] = Kleisli[m, a, b]
        override val selfArrow = _asArrow(i)
        override def left[b, c, d](f: a[b, c], * : Type[d] = null): a[Either[b, d], Either[c, d]] = f +++: arr(id[d])
        override def right[b, c, d](f: a[b, c], * : Type[d] = null): a[Either[d, b], Either[d, c]] = arr(id[d]) +++: f
        override def op_+++:[b, c, b_, c_](f: a[b, c])(g: a[b_, c_]): a[Either[b, b_], Either[c, c_]] = (f >>>: arr(Left(_: c).of[c, c_])) |||: (g >>>: arr(Right(_: c_).of[c, c_]))
        override def op_|||:[b, c, d](f: a[b, d])(g: a[c, d]): a[Either[b, c], d] = Kleisli { Either.either(f.run)(g.run) }
    }

    implicit def _asArrowLoop[m[+_]](implicit i: MonadFix[m]): ArrowLoop[apply1[m]#apply2] = new ArrowLoop[apply1[m]#apply2] with ArrowProxy[apply1[m]#apply2] {
        private type a[-a, +b] = Kleisli[m, a, b]
        override val selfArrow = _asArrow(i)
        override def loop[b, c, d](f: a[(b, Lazy[d]), (Lazy[c], Lazy[d])]): a[b, c] = {
            def f_(x: b)(y: Lazy[(c, d)]): m[(c, d)] = {
                import i.`for`
                for { (c, d) <- (f.run)(x, Lazy(Pair.snd(y))) } yield (c.!, d.!)
            }
            Kleisli { i.liftM[(c, d), c](Pair.fst)_ `.` i.mfix[(c, d)] `.` f_ }
        }
    }
}

private[ken] sealed trait KleisliAs extends KleisliAs0 { this: Kleisli.type =>
    implicit def _asArrow[m[+_]](implicit i: Monad[m]): ArrowApply[apply1[m]#apply2] = new ArrowApply[apply1[m]#apply2] {
        import i.{>>=, `return`}
        // Category
        private type cat[-a, +b] = Kleisli[m, a, b]
        override def cid[a]: cat[a, a] = Kleisli { a => `return`(a) }
        override def op_<<<:[a, b, c](f: cat[b, c])(g: cat[a, b]): cat[a, c] = Kleisli { b =>
            g.run(b) >>= f.run
        }
        // Arrow
        private type a[-a, +b] = Kleisli[m, a, b]
        override def arr[b, c](f: b => c): a[b, c] = Kleisli { b => `return`(f(b)) }
        override def first[b, c, d](f: a[b, c], * : Type[d] = null): a[(b, d), (c, d)] = Kleisli { case (b, d) =>
            (f.run)(b) >>= (c => `return`(c, d))
        }
        override def second[b, c, d](f: a[b, c], * : Type[d] = null): a[(d, b), (d, c)] = Kleisli { case (d, b) =>
            (f.run)(b) >>= (c => `return`(d, c))
        }
        // ArrowApply
        override def app[b, c]: a[(a[b, c], b), c] = Kleisli { case (f, x) => (f.run)(x) }
    }
}
