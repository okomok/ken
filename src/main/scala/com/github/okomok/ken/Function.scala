

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


object Function extends ArrowChoice[Function] with ArrowApply[Function] with ArrowLoop[Function] with Kind.qcurry2[Function] {
    def fix[a](f: Lazy[a] => a): a = {
        lazy val x: a = f(x)
        x
    }

    def fixfun[a](f: (a => a) => a => a): a => a = { x => f(fixfun(f))(x) }

    def on[a, b, c](* : b => b => c)(f: a => b): a => a => c = { x => y => *(f(x))(f(y)) }

    // Overrides
    //
    // Category
    private type cat[-a, +b] = a => b
    override def cid[a]: cat[a, a] = id[a]
    override def op_<<<:[a, b, c](f: cat[b, c])(g: cat[a, b]): cat[a, c] = f `.` g
    // Arrow
    private type a[-a, +b] = a => b
    override def arr[b, c](f: b => c): a[b, c] = f
    override def first[b, c, d](f: a[b, c], * : Type[d] = null): a[(b, d), (c, d)] = f ***: id[d]
    override def second[b, c, d](f: a[b, c], * : Type[d] = null): a[(d, b), (d, c)] = id[d] ***: f
    override def op_***:[b, c, b_, c_](f: a[b, c])(g: a[b_, c_]): a[(b, b_), (c, c_)] = { case (x, y) => (f(x), g(y)) }
    // ArrowChoice
    override def left[b, c, d](f: a[b, c], * : Type[d] = null): a[Either[b, d], Either[c, d]] = f +++: id[d]
    override def right[b, c, d](f: a[b, c], * : Type[d] = null): a[Either[d, b], Either[d, c]] = id[d] +++: f
    override def op_+++:[b, c, b_, c_](f: a[b, c])(g: a[b_, c_]): a[Either[b, b_], Either[c, c_]] = ((x: b) => Left(f(x)).of[c, c_]) |||: ((x: b_) => Right(g(x)).of[c, c_])
    override def op_|||:[b, c, d](f: a[b, d])(g: a[c, d]): a[Either[b, c], d] = Either.either(f)(g)
    // ArrowApply
    override def app[b, c]: a[(a[b, c], b), c] = { case (f, x) => f(x) }
    // ArrowLoop
    override def loop[b, c, d](f: a[(b, Lazy[d]), (Lazy[c], Lazy[d])]): a[b, c] = b => {
        new { val t: (Lazy[c], Lazy[d]) = f(b, Lazy(t._2.!)) }.t._1.!
        //lazy val t: (c, d) = f(b, Lazy(t._2)) // scalac CRASH.
        //t._1
    }

    // Instances
    //
    private[ken] def _asMonadReader[z]: MonadReader[z, ({type m[+a] = z => a})#m] = new MonadReader[z, ({type m[+a] = z => a})#m] {
        // Functor
        private type f[+a] = z => a
        override def fmap[a, b](x: a => b)(y: f[a]): f[b] = x `.` y
        // Applicative
        override def pure[a](x: Lazy[a]): f[a] = const(x)
        override def op_<*>[a, b](x: f[a => b])(y: f[a]): f[b] = { z => x(z)(y(z)) }
        // Monad
        private type m[+a] = f[a]
        override def `return`[a](x: Lazy[a]): m[a] = const(x)
        override def op_>>=[a, b](f: m[a])(k: a => m[b]): m[b] = { z => k(f(z))(z) }
        // MonadReader
        override def ask: m[z] = id
        override def local[a](f: z => z)(m: m[a]): m[a] = m `.` f
    }

    private[ken] def _asMonoid[z, b](implicit mb: Monoid[b]): Monoid[z => b] = new Monoid[z => b] {
        private type m = z => b
        override val mempty: m = _ => mb.mempty
        override val mappend: m => Lazy[m] => m = { x => y =>
            val y_ = y // scalac mistery
            z => mb.mappend(x(z))(y_(z))
        }
    }
}
