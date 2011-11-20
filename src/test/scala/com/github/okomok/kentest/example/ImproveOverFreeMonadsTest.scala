

// Public domain


package com.github.okomok.kentest.example


import com.github.okomok.ken._


// Asymptotic Improvement of Computations over Free Monads (mpc08.pdf)


class ImproveOverFreeMonadsTest extends org.scalatest.junit.JUnit3Suite {

    // 2. A Specific Example

    sealed abstract class Tree[+a] extends Up[Tree[a]]
    final case class Leaf[a](_1: a) extends Tree[a]
    final case class Node[a](_2: Tree[a], _3: Tree[a]) extends Tree[a]

    object Tree extends TreeLike[Tree] with ThisIsInstance {
        def subst[a, b](t: Tree[a])(k: a => Tree[b]): Tree[b] = t match {
            case Leaf(a) => k(a)
            case Node(t1, t2) => Node(subst(t1)(k), subst(t2)(k))
        }

        // as Monad
        private type m[+a] = Tree[a]
        override def `return`[a](x: Lazy[a]): m[a] = Leaf(x)
        override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = subst(m)(k)

        // as TreeLike
        override def node[a](l: m[a])(r: m[a]): m[a] = Node(l, r)
    }

    lazy val fullTree: Int => Tree[Int] = {
        case 1 => Leaf(1)
        case n => for { i <- fullTree(n-1) } Node(Leaf(n-1-i), Leaf(i+1))
    }

    lazy val zigzag: Tree[Int] => Int = {
        lazy val zig: Tree[Int] => Int = {
            case Leaf(n) => n
            case Node(t1, t2) => zag(t1)
        }
        lazy val zag: Tree[Int] => Int = {
            case Leaf(n) => n
            case Node(t1, t2) => zig(t2)
        }
        zig
    }

    // Codensity of Tree
    trait CTree[+a] {
        def apply[b](h: a => Tree[b]): Tree[b] // cps-style makes >>= right-associative.
    }

    object CTree extends TreeLike[CTree] with ThisIsInstance {
        // as Monad
        private type m[+a] = CTree[a]
        override def `return`[a](a: Lazy[a]): m[a] = new CTree[a] {
            override def apply[b](h: a => Tree[b]): Tree[b] = h(a)
        }
        override def op_>>=[a, b](p: m[a])(k: a => m[b]): m[b] = new CTree[b] {
            override def apply[c](h: b => Tree[c]): Tree[c] = p { (a: a) => k(a)(h) }
        }

        // as TreeLike
        override def node[a](l: m[a])(r: m[a]): m[a] = new CTree[a] {
            override def apply[b](h: a => Tree[b]): Tree[b] = Node(l(h), r(h))
        }
    }

    def rep[a](t: Tree[a]): CTree[a] = new CTree[a] {
        override def apply[b](h: a => Tree[b]): Tree[b] = t >>= h
    }

    def abs[a](ct: CTree[a]): Tree[a] = ct { (a: a) => Leaf(a) }

    trait TreeLike[m[+_]] extends Monad[m] {
        def node[a](l: m[a])(r: m[a]): m[a]
    }

    def leaf[m[+_],a](a: a)(implicit i: TreeLike[m]): m[a] = i.`return`(a)

    def fullTree_[m[+_]](n: Int)(implicit j: TreeLike[m]): m[Int] = {
        if (n == 1) leaf(1)
        else {
            import j.`for`
            for { i <- fullTree_(n-1) } { j.node(leaf(n-1-i))(leaf(i+1)) }
        }
    }

    trait CanImprove[a] {
        def apply[m[+_]](implicit i: TreeLike[m]): m[a]
    }

    def improve[m[+_], a](m: CanImprove[a]): Tree[a] = abs { m(CTree) }

    // cps-style is right-associative.
    def fullTree__[b](n: Int)(h: Int => Tree[b]): Tree[b] = {
        if (n == 1) h(1)
        else {
            fullTree__(n-1)(i => Node(h(n-1-i), h(i+1)))
        }
    }

    def testTrivial {
        zigzag(fullTree_[Tree](3))
        fullTree_[CTree](3)
        zigzag(fullTree__(3)(Leaf(_)))

        zigzag(fullTree_[Tree](3))
        zigzag(abs(fullTree_[CTree](3)))
        zigzag(improve {
            new CanImprove[Int] {
                override def apply[m[+_]](implicit i: TreeLike[m]): m[Int] = fullTree_(3)
            }
        })
    }

    sealed abstract class Free[+f[+_], +a]
    final case class Return[a](_1: a) extends Free[Nothing, a]
    final case class Wrap[f[+_], a](_1: f[Free[f, a]]) extends Free[f, a]

    object Free {
        trait apply1[f[+_]] extends Kind.Function1 {
            override type apply1[+a] = Free[f, a]
        }

        implicit def _asMonad[f[+_]](implicit i: Functor[f]): Monad[apply1[f]#apply1] = new Monad[apply1[f]#apply1] {
            private type m[+a] = Free[f, a]
            override def `return`[a](x: Lazy[a]): m[a] = Return(x)
            override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = m match {
                case Return(a) => k(a)
                case Wrap(m) => Wrap { i.fmap((f: Free[f, a]) => f >>= k)(m) }
            }
        }

        implicit def _asFreeLike[f[+_]](implicit i: Functor[f]): FreeLike[f, apply1[f]#apply1] = new FreeLike[f, apply1[f]#apply1] with MonadProxy[apply1[f]#apply1] {
            private type m[+a] = Free[f, a]
            override val selfMonad = _asMonad[f](i)
            override val functor = i
            override def wrap[a](fs: f[m[a]]): m[a] = Wrap(fs)
        }
    }

    // type Tree === Free[F]
    final case class F[+b](l: b, r: b)

    object F extends Functor[F] with ThisIsInstance {
        private type f[+a] = F[a]
        override def fmap[a, b](k: a => b): f[a] => f[b] = {
            case F(x, y) => F(k(x), k(y))
        }
    }

    // Codensity
    trait C[f[+_], +a] {
        def apply[r](k: a => f[r]): f[r]
    }

    object C {
        trait apply1[f[+_]] extends Kind.Function1 {
            override type apply1[+a] = C[f, a]
        }

        implicit def _asMonad[f[+_]]: Monad[apply1[f]#apply1] = new Monad[apply1[f]#apply1] {
            private type m[+a] = C[f, a]
            override def `return`[a](x: Lazy[a]): m[a] = new C[f, a] {
                override def apply[r](k: a => f[r]): f[r] = k(x)
            }
            override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = new C[f, b] {
                override def apply[r](c: b => f[r]): f[r] = m(a => k(a)(c))
            }
        }

        implicit def _asFreeLike[f[+_], n[+_]](implicit i: FreeLike[f, n]): FreeLike[f, apply1[n]#apply1] = new FreeLike[f, apply1[n]#apply1] with MonadProxy[apply1[n]#apply1] {
            private type m[+a] = C[n, a]
            override val selfMonad = _asMonad[n]
            override val functor = i.functor
            override def wrap[a](t: f[m[a]]): m[a] = new C[n, a] {
                override def apply[r](h: a => n[r]): n[r] = i.wrap {
                    import i.functor.`for`
                    for { p <- t } yield p(h)
                }
            }
        }

        def rep[f[+_], a](m: f[a])(implicit i: Monad[f]): C[f, a] = new C[f, a] {
            override def apply[r](k: a => f[r]): f[r] = i.op_>>=(m)(k)
        }

        def abs[f[+_], a](m: C[f, a])(implicit i: Monad[f]): f[a] = m(x => i.`return`(x))
    }


    trait FreeLike[f[+_], m[+_]] extends Monad[m] {
        val functor: Functor[f]
        def wrap[a](fs: f[m[a]]): m[a]
    }


}
