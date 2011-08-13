

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


private[ken] final class _Kleislis[m[+_]](val monad: Monad[m]) {
    final case class _Kleisli[-a, +b](override val get: a => m[b]) extends Strong[a => m[b]]

    object _Kleisli extends Instance

    private[ken] trait Instance { this: _Kleisli.type =>
        implicit val _asArrow: Arrow[_Kleisli] = new Arrow[_Kleisli] {
            import monad.{>>=, `return`}

            private[this] type cat[-a, +b] = _Kleisli[a, b]
            override def cid[a]: cat[a, a] = _Kleisli { a => `return`(a) }
            override def op_<<<[a, b, c](f: cat[b, c])(g: cat[a, b]): cat[a, c] = _Kleisli { b =>
                g.run(b) >>= f.run
            }

            private[this] type a[-a, +b] = _Kleisli[a, b]
            override def arr[b, c](f: b => c): a[b, c] = _Kleisli { b => `return`(f(b)) }
            override def first[b, c, d](f: a[b, c]): a[(b, d), (c, d)] = _Kleisli { case (b, d) =>
                f.run(b) >>= (c => `return`(c, d))
            }
            override def second[b, c, d](f: a[b, c]): a[(d, b), (d, c)] = _Kleisli { case (d, b) =>
                f.run(b) >>= (c => `return`(d, c))
            }
        }

        implicit def _asArrowPlus(implicit i: MonadPlus[m]): ArrowPlus[_Kleisli] = new ArrowPlus[_Kleisli] with ArrowProxy[_Kleisli] {
            private[this] type a[a, b] = _Kleisli[a, b]
            override val self = _asArrow
            override def zeroArrow[b, c]: a[b, c] = _Kleisli { _ => i.mzero }
            override def op_<+>[b, c](f: a[b, c])(g: => a[b, c]): a[b, c] = _Kleisli { x =>
                import i._mplus_
                f.run(x) _mplus_ g.run(x)
            }
        }
    }
}
