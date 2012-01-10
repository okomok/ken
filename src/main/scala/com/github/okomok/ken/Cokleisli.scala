

// Copyright Shunsuke Sogame 2011.
//
// Copyright 2008-2011 Edward Kmett
// Copyright 2004-2008 Dave Menendez
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


final case class Cokleisli[w[+_], -a, +b](override val old: w[a] => b) extends NewtypeOf[w[a] => b] with (w[a] => b) {
    override def apply(x: w[a]): b = old(x)
}


object Cokleisli extends CokleisliOp with CokleisliAs with Kind.FunctionLike {
    trait apply[w <: Kind.Function1] extends apply1[w]
    trait apply1[w <: Kind.Function1] extends Kind.Newtype2 {
        override type apply2[-a, +b] = Cokleisli[w#apply1, a, b]
        override type oldtype2[-a, +b] = w#apply1[a] => b
    }

    trait apply2[w <: Kind.Function1, a] extends Kind.Function1 {
        override type apply1[+b] = Cokleisli[w#apply1, a, b]
    }
}


private[ken] sealed trait CokleisliOp { this: Cokleisli.type =>
    def run[w[+_], a, b](f: Cokleisli[w, a, b]): w[a] => b = f.run
}


private[ken] sealed trait CokleisliAs { this: Cokleisli.type =>
/*
    implicit def _asNewtype2[w[+_]]: Newtype2[({type L[-a, +b] = Cokleisli[w, a, b]})#L, ({type ot[-a, +b] = w[a] => b})#ot] = new Newtype2[({type L[-a, +b] = Cokleisli[w, a, b]})#L, ({type ot[-a, +b] = w[a] => b})#ot] {
        private type nt[-a, +b] = Cokleisli[w, a, b]
        private type ot[-a, +b] = w[a] => b
        override def newOf[a, b](ot: Lazy[ot[a, b]]): nt[a, b] = Cokleisli(ot)
        override def oldOf[a, b](nt: Lazy[nt[a, b]]): ot[a, b] = nt.run
    }
*/
    implicit def _asArrow[w[+_]](implicit i: Comonad[w]): ArrowApply[({type L[-a, +b] = Cokleisli[w, a, b]})#L] with ArrowChoice[({type L[-a, +b] = Cokleisli[w, a, b]})#L] = new ArrowApply[({type L[-a, +b] = Cokleisli[w, a, b]})#L] with ArrowChoice[({type L[-a, +b] = Cokleisli[w, a, b]})#L] {
        // Category
        private type cat[-a, +b] = Cokleisli[w, a, b]
        override def cid[a]: cat[a, a] = Cokleisli { (w: w[a]) => i.extract(w) }
        override def op_<<<:[a, b, c](f: cat[b, c])(g: cat[a, b]): cat[a, c] = Cokleisli {
            import i.=<=:
            f.run =<=: g.run
        }
        // Arrow
        private type a[-a, +b] = Cokleisli[w, a, b]
        override def arr[b, c](f: b => c): a[b, c] = Cokleisli { (w: w[b]) => f(i.extract(w)) }
        override def first[b, c, d](f: a[b, c], * : Type[d] = null): a[(b, d), (c, d)] = f ***: cid
        override def second[b, c, d](f: a[b, c], * : Type[d] = null): a[(d, b), (d, c)] = cid[d] ***: f
        override def op_***:[b, c, b_, c_](f: a[b, c])(g: a[b_, c_]): a[(b, b_), (c, c_)] = Cokleisli {
            val fa = Arrow[Function.type]
            import fa.{<<<:, &&&:}
            ((f.run <<<: i.fmap(Pair.fst[b])): w[(b, b_)] => c) &&&: ((g.run <<<: i.fmap(Pair.snd[b_])): w[(b, b_)] => c_)
        }
        override def op_&&&:[b, c, c_](f: a[b, c])(g: a[b, c_]): a[b, (c, c_)] = Cokleisli {
            val fa = Arrow[Function.type]
            import fa.&&&:
            f.run &&&: g.run
        }
        // ArrowApply
        override def app[b, c]: a[(a[b, c], b), c] = Cokleisli { (w: w[(a[b, c], b)]) =>
            import i.<@>
            run(Pair.fst(i.extract(w)))(Function.from(Pair.snd[b]) <@> w)
        }
        // ArrowChoice
        override def left[b, c, d](f: a[b, c], * : Type[d] = null): a[Either[b, d], Either[c, d]] = leftApp(f, *)
    }

    implicit def _asMonad[w[+_], z]: Monad[({type L[+a] = Cokleisli[w, z, a]})#L] = new Monad[({type L[+a] = Cokleisli[w, z, a]})#L] {
        // Functor
        private type f[+a] = Cokleisli[w, z, a]
        override def fmap[a, b](f: a => b): f[a] => f[b] = g => Cokleisli { f `.` g.run }
        // Applicative
        override def pure[a](x: Lazy[a]): f[a] = Cokleisli(const(x))
        override def op_<*>[a, b](f: f[a => b]): f[a] => f[b] = a => Cokleisli { (w: w[z]) => (f.run)(w)((a.run)(w)) }
        // Monad
        private type m[+a] = Cokleisli[w, z, a]
        override def op_>>=[a, b](k: m[a])(f: a => m[b]): m[b] = Cokleisli { (w: w[z]) => (f((k.run)(w)).run)(w) }
    }
}
