

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


private[ken] final class _Kleislis[m[+_]](val monad: Monad[m]) {
    final case class _Kleisli[-a, +b](override val get: a => m[b]) extends Strong[a => m[b]]

    object _Kleisli extends Instance {
        def run[a, b](f: _Kleisli[a, b]): a => m[b] = f.run

        implicit def from[a, b](n: Strong[a => m[b]]): _Kleisli[a, b] = _Kleisli { n.run }
    }

    private[ken] trait Instance0 { this: _Kleisli.type =>
        implicit val _asArrow: Arrow[_Kleisli] = new Arrow[_Kleisli] {
            import monad.{>>=, `return`}
            // Category
            private[this] type cat[-a, +b] = _Kleisli[a, b]
            override def cid[a]: cat[a, a] = _Kleisli { a => `return`(a) }
            override def op_<<<[a, b, c](f: cat[b, c])(g: cat[a, b]): cat[a, c] = _Kleisli { b =>
                g.run(b) >>= f.run
            }
            // Arrow
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
            private[this] type a[-a, +b] = _Kleisli[a, b]
            override def self = _asArrow
            override def zeroArrow[b, c]: a[b, c] = _Kleisli { _ => i.mzero }
            override def op_<+>[b, c](f: a[b, c])(g: => a[b, c]): a[b, c] = _Kleisli { x =>
                import i._mplus_
                f.run(x) _mplus_ g.run(x)
            }
        }
    }

    private[ken] trait Instance1 extends Instance0 { this: _Kleisli.type =>
        implicit val _asArrowChoice: ArrowChoice[_Kleisli] = new ArrowChoice[_Kleisli] with ArrowProxy[_Kleisli] {
            private[this] type a[-a, +b] = _Kleisli[a, b]
            override def self = _asArrow
            override def left[b, c, d](f: a[b, c]): a[Either[b, d], Either[c, d]] = f +++ arr(id[d])
            override def right[b, c, d](f: a[b, c]): a[Either[d, b], Either[d, c]] = arr(id[d]) +++ f
            override def op_+++[b, c, b_, c_](f: a[b, c])(g: a[b_, c_]): a[Either[b, b_], Either[c, c_]] = (f >>> arr(Left(_: c).of[c, c_])) ||| (g >>> arr(Right(_: c_).of[c, c_]))
            override def op_|||[b, c, d](f: a[b, d])(g: a[c, d]): a[Either[b, c], d] = _Kleisli { Either.either(f.run)(g.run) }
        }
    }

    private[ken] trait Instance2 extends Instance1 { this: _Kleisli.type =>
        implicit val _asArrowApply: ArrowApply[_Kleisli] = new ArrowApply[_Kleisli] with ArrowProxy[_Kleisli] {
            private[this] type a[-a, +b] = _Kleisli[a, b]
            override def self = _asArrow
            override def app[b, c]: a[(a[b, c], b), c] = _Kleisli { case (f, x) => f.run(x) }
        }
    }

    private[ken] trait Instance extends Instance2 { this: _Kleisli.type =>
        implicit def _asArrowLoop(implicit i: MonadFix[m]): ArrowLoop[_Kleisli] = new ArrowLoop[_Kleisli] with ArrowProxy[_Kleisli] {
            private[this] type a[-a, +b] = _Kleisli[a, b]
            override def self = _asArrow
            override def loop[b, c, d](f: a[(b, d), (c, d)]): a[b, c] = {
                def f_(x: b)(y: => (c, d)): m[(c, d)] = f.run(x, snd(y))
                _Kleisli { i.liftM[(c, d), c](fst)_ compose i.mfix[(c, d)] compose f_ }
            }
        }
    }
}
