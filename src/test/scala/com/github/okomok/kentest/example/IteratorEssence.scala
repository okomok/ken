// Public domain

package com.github.okomok.kentest.example

import com.github.okomok.ken._

// The Essence of the Iterator Pattern

class IteratorEssenceTest extends org.scalatest.junit.JUnit3Suite {

    // 3.1 Monadic applicative functors
    //

    final case class Stream[+a](head: a, tail: Lazy[Stream[a]])

    object Stream extends Kind.quote1[Stream] {
        implicit def _asApplicative[a]: Applicative[Stream] = new Applicative[Stream] {
            private type f[+a] = Stream[a]
            override def pure[a](x: Lazy[a]): f[a] = {
                lazy val xs: Stream[a] = Stream(x, xs)
                xs
            }
            override def op_<*>[a, b](fs: f[a => b]): f[a] => f[b] = xs => (fs, xs) match {
                case (Stream(f, fs), Stream(x, xs)) => Stream(f(x), fs.! <*> xs.!)
            }
        }
    }

    def testStream {
        val _as = Applicative[Stream.type]
        import _as._
        // pointwise "zip with apply"
        val res = ((x: Int) => (y: Int) => x + y) <@> Stream(3, pure(4)) <*> Stream(10, pure(20))
        expect(13)(res.head)
        expect(24)(res.tail.head)
    }

    // 3.2 Monoidal applicative functors
    //
    // See: ken.Const, ZipList

    // 3.3 Combining applicative functors
    //

    // Product (parallell composition)
    final case class Prod[m[+_], n[+_], +a](_1: m[a], _2: n[a])

    object Prod {
        trait apply2[m[+_], n[+_]] extends Kind.Function1 {
            override type apply1[+a] = Prod[m, n, a]
        }

        def prod[m[+_], n[+_], a, b](f: a => m[b])(g: a => n[b]): a => Prod[m, n, b] = x => Prod(f(x), g(x))

        implicit def _asApplicative[m[+_], n[+_]](implicit _Am: Applicative[m], _An: Applicative[n]): Applicative[apply2[m, n]#apply1] = new Applicative[apply2[m, n]#apply1] {
            private type f[+a] = Prod[m, n, a]
            override def pure[a](x: Lazy[a]): f[a] = Prod(_Am.pure(x), _An.pure(x))
            override def op_<*>[a, b](mf: f[a => b]): f[a] => f[b] = mx => Prod(_Am.op_<*>(mf._1)(mx._1), _An.op_<*>(mf._2)(mx._2))
        }
    }

    // Composition (sequential composition)
    final case class Comp[m[+_], n[+_], +a](_1: m[n[a]])

    object Comp {
        trait apply2[m[+_], n[+_]] extends Kind.Function1 {
            override type apply1[+a] = Comp[m, n, a]
        }

        def comp[m[+_], n[+_], a, b, c](f: b => n[c])(g: a => m[b])(implicit _Fm: Functor[m]): a => Comp[m, n, c] = x => Comp(_Fm.fmap(f)(g(x)))

        implicit def _asApplicative[m[+_], n[+_]](implicit _Am: Applicative[m], _An: Applicative[n]): Applicative[apply2[m, n]#apply1] = new Applicative[apply2[m, n]#apply1] {
            private type f[+a] = Comp[m, n, a]
            override def pure[a](x: Lazy[a]): f[a] = Comp(_Am.pure(_An.pure(x)))
            override def op_<*>[a, b](mf: f[a => b]): f[a] => f[b] = mx => (mf, mx) match {
                case (Comp(mf), Comp(mx)) => Comp( _Am.op_<*>( _Am.op_<*>(_Am.pure(_An.op_<*>[a, b]_): m[n[a => b] => n[a] => n[b]])(mf: m[n[a => b]]): m[n[a] => n[b]] )(mx: m[n[a]]): m[n[b]] )
            }
        }
    }

    // 3.4 Idiomatic traversal
    //

    def traverseList[m[+_], a, b](f: a => m[b])(xs: List[a])(implicit _Am: Applicative[m]): m[List[b]] = xs match {
        case Nil => _Am.pure(Nil)
        case x :: xs => {
            import _Am.{<@>, <*>}
            List.op_!::[b]_ <@> f(x) <*> traverseList(f)(xs)
        }
    }

    def distList[m[+_], a](xs: List[m[a]])(implicit _Am: Applicative[m]): m[List[a]] = traverseList(id[m[a]])(xs)

    sealed abstract class Tree[+a]
    final case class Leaf[a](_1: a) extends Tree[a]
    final case class Bin[a](_1: Tree[a], _2: Tree[a]) extends Tree[a]

    object Tree extends Kind.quote1[Tree] {
        implicit val _asTraversable: Traversable[Tree] = new Traversable[Tree] {
            private type t[+a] = Tree[a]
            override def traverse[f[+_], a, b](f: a => f[b])(t: t[a])(implicit _Af: Applicative[f]): f[t[b]] = {
                import _Af.{<@>, <*>}
                t match {
                    case Leaf(x) => ((b: b) => Leaf(b)) <@> f(x)
                    case Bin(l, r) => ((bl: Tree[b]) => (br: Tree[b]) => Bin(bl, br)) <@> traverse(f)(l) <*> traverse(f)(r)
                }
            }
        }
    }

    // reduce =:= Foldable.foldMap

    // 4. Traversables as iterators
    //

    trait Coerce[a, b] {
        type down = a => b
        def down: down
        type up = b => a
        def up: up
    }

    object Coerce {
        implicit def _ofIdentity[a]: Coerce[Identity[a], a] = new Coerce[Identity[a], a] {
            override val down: down = x => x.old
            override val up: up = x => Identity(x)
        }

        implicit def _ofConst[a, b]: Coerce[Const[a, b], a] = new Coerce[Const[a, b], a] {
            override val down: down = x => x.old
            override val up: up = x => Const(x)
        }

        implicit def _ofProd[m[+_], n[+_], a, b, c](implicit _C1: Coerce[m[a], b], _C2: Coerce[n[a], c]): Coerce[Prod[m, n, a], (b, c)] = new Coerce[Prod[m, n, a], (b, c)] {
            override val down: down = mnx => (_C1.down(mnx._1), _C2.down(mnx._2))
            override val up: up = { case (x, y) => Prod(_C1.up(x), _C2.up(y)) }
        }

        implicit def _ofComp[m[+_], n[+_], a, b, c](implicit _Fm: Functor[m], _Fn: Functor[n], _C1: Coerce[m[b], c], _C2: Coerce[n[a], b]): Coerce[Comp[m, n, a], c] = new Coerce[Comp[m, n, a], c] {
            override val down: down = x => _C1.down(_Fm.fmap(_C2.down)(x._1))
            override val up: up = x => Comp(_Fm.fmap(_C2.up)(_C1.up(x)))
        }

        implicit def _ofMaybe[a]: Coerce[Maybe[a], Maybe[a]] = new Coerce[Maybe[a], Maybe[a]] {
            override val down: down = id
            override val up: up = id
        }

        implicit def _ofState[s, a]: Coerce[State[s, a], s => (a, s)] = new Coerce[State[s, a], s => (a, s)] {
            override val down: down = x => x.old
            override val up: up = x => State(x)
        }
    }

    // 4.1 Shape and contents
    //

    // `Const` makes a trivial Applicative for a Monoid.
    def contentsBody[a](x: a): Const[List[a], Nothing] = Const(List(x))

    // In case `Monoid[a]` missing, you need a List wrapping around.
    def contents[t[+_], a](xs: t[a])(implicit _Tt: Traversable[t]): Const[List[a], t[Nothing]] = {
        val _Ac = Applicative[Const.apply[List[a]]]
        _Tt.traverse[_Ac.apply, a, Nothing](a => contentsBody(a))(xs)(_Ac)
    }

    def run[t[+_], a, b, c](program: t[a] => b)(xs: t[a])(implicit _C: Coerce[b, c], _T: Traversable[t]): c = _C.down(program(xs))

    def runContents[t[+_], a](xs: t[a])(implicit _T: Traversable[t]): List[a] = run((xs: t[a]) => contents(xs))(xs)

    def runContents2[t[+_], a](xs: t[a])(implicit _Tt: Traversable[t]): List[a] = _Tt.foldMap((a: a) => List(a))(xs)

    def testContents {
        expect(List(1,2,3))(runContents(List(1,2,3)))
        expect(List(1,2,3))(runContents2(List(1,2,3)))
    }

    lazy val shapeBody: Any => Identity[Unit] = _ => Identity()

    def shape[t[+_], a](xs: t[a])(implicit _T: Traversable[t]): Identity[t[Unit]] = _T.traverse(shapeBody)(xs)

    def testShape {
        expect(List((),(),()))(shape(List(1,2,3)).old)
    }

    def decompose2pass[t[+_], a](xs: t[a])(implicit _T: Traversable[t]): Prod[Identity, Const.apply[List[a]]#apply, t[Unit]] = {
        Prod[Identity, Const.apply[List[a]]#apply, t[Unit]](shape(xs), contents(xs))
    }

    // single-pass
    def decompose[t[+_], a](xs: t[a])(implicit _T: Traversable[t]): Prod[Identity, Const.apply[List[a]]#apply, t[Unit]] = {
        implicit val _Ac = Applicative[Const.apply[List[a]]]
        implicit val _Ap = Prod._asApplicative[Identity, _Ac.apply]
        _T.traverse[Prod.apply2[Identity, Const.apply[List[a]]#apply]#apply1, a, Unit](a => Prod[Identity, Const.apply[List[a]]#apply, Unit](shapeBody(a), contentsBody(a)))(xs)
    }

    def testDecompose2pass {
        val p = decompose2pass(List(1,2,3))
        expect(List((),(),()))(p._1.old)
        expect(List(1,2,3))(p._2.old)
    }

    def testDecompose1pass {
        val p = decompose(List(1,2,3))
        expect(List((),(),()))(p._1.old)
        expect(List(1,2,3))(p._2.old)
    }

    def reassembleBody[a]: Unit => Comp[State.apply[List[a]]#apply, Maybe, a] = _ => {
        val takeHead: Unit => List[a] => (Maybe[a], List[a]) = _ => {
            case Nil => (Nothing, Nil)
            case y :: ys => (Just(y), ys)
        }
        Comp[State.apply[List[a]]#apply, Maybe, a](State(takeHead()))
    }

    // Seems a folding. state(List[a] in this case) behaves like a seed of folding.
    def reassemble[t[+_], a](xs: t[Unit])(implicit _T: Traversable[t]): Comp[State.apply[List[a]]#apply, Maybe, t[a]] = {
        implicit val _As = Applicative[State.apply[List[a]]]
        implicit val _Ac = Applicative[Comp.apply2[_As.apply, Maybe]]
        _T.traverse[_Ac.apply, Unit, a](reassembleBody[a])(xs)
    }

    def runReassemble[t[+_], a](x: (t[Unit], List[a]))(implicit _T: Traversable[t]): Maybe[t[a]] = {
        val run_reassemble: t[Unit] => List[a] => (Maybe[t[a]], List[a]) = xs => reassemble(xs)._1.run
        Pair.fst(Pair.uncurry(run_reassemble)(x))
    }

    def testRunReassemble {
        val res = runReassemble((List((),(),()), List(1,2,3,4,5)))
        expect(Just(List(1,2,3)))(res)
        val res0 = runReassemble(List((),(),()), Nil)
        expect(Nothing)(res0)
        val res0b = runReassemble(Nil, List(1,2,3))
        expect(Just(Nil))(res0b)
    }

    // 4.2 Collection and dispersal
    //

    def collect[t[+_], m[+_], a, b](f: a => m[Unit])(g: a => b)(xs: t[a])(implicit _T: Traversable[t], _A: Applicative[m]): m[t[b]] = {
        import _A.<@>
        _T.traverse((a: a) => ((u: Unit) => g(a)) <@> f(a))(xs)
    }

    def loop[t[+_], a, b](touch: a => b)(xs: t[a])(implicit _T: Traversable[t]): State[Int, t[b]] = {
        val _M = MonadState[Int, State.apply[Int]]
        import _M._
        collect((a: a) => for { n <- get } put(n+1))(touch)(xs)
    }

    def testLoop {
        val (xs, n) = loop((a: Int) => a + 1)(List(1,2,3)).apply(10)
        expect(13)(n)
        expect(List(2,3,4))(xs)
    }

    def disperse[t[+_], m[+_], a, b, c](mb: m[b])(g: a => b => c)(xs: t[a])(implicit _T: Traversable[t], _A: Applicative[m]): m[t[c]] = {
        import _A.<@>
        _T.traverse((a: a) => g(a) <@> mb)(xs)
    }

    def label[t[+_], a](xs: t[a])(implicit _T: Traversable[t]): State[Int, t[Int]] = {
        implicit val _A = Applicative[State.apply[Int]]
        disperse(_A.infer(step))(Pair.curry(Pair.snd)/*ignores elements of xs*/)(xs)
    }

    lazy val step: State[Int, Int] = {
        val _M = MonadState[Int, State.apply[Int]]
        import _M._
        for { n <- get; _ <- put(n+1) } yield n
    }

    def testLabel {
        val (ns, n) = label(List(3,6,9)).apply(10)
        expect(List(10,11,12))(ns)
        expect(13)(n)
    }

    // 4.3. Backward traversal
    //

    final case class Backwards[n[+_], +a](override val old: n[a]) extends NewtypeOf[n[a]]

    object Backwards {
        trait apply1[n[+_]] extends Kind.Function1 {
            override type apply1[+a] = Backwards[n, a]
        }

        implicit def _asApplicative[n[+_], a](implicit _A: Applicative[n]): Applicative[apply1[n]#apply1] = new Applicative[apply1[n]#apply1] {
            private type f[+a] = Backwards[n, a]
            override def pure[a](x: Lazy[a]): f[a] = Backwards(_A.pure(x))
            override def op_<*>[a, b](f: f[a => b]): f[a] => f[b] = x => {
                Backwards( _A.op_<*>(_A.op_<@>(((a: a) => (g: a => b) => g(a)))(x.old))(f.old) )
            }
        }
    }

    final case class Forwards[n[+_], +a](override val old: n[a]) extends NewtypeOf[n[a]]

    object Forwards {
        trait apply1[n[+_]] extends Kind.Function1 {
            override type apply1[+a] = Forwards[n, a]
        }

        implicit def _asApplicative[n[+_], a](implicit _A: Applicative[n]): Applicative[apply1[n]#apply1] = new Applicative[apply1[n]#apply1] {
            private type f[+a] = Forwards[n, a]
            override def pure[a](x: Lazy[a]): f[a] = Forwards(_A.pure(x))
            override def op_<*>[a, b](f: f[a => b]): f[a] => f[b] = x => {
                Forwards( _A.op_<*>(f.old)(x.old) )
            }
        }
    }

    // 5. Laws of traverse
    //

    // 6 Modular programming with applicative functors
    //

    import Monoid.Sum

    type Count[+a] = Const[Sum[Int], a]

    lazy val count: Any => Count[Nothing] = _ => Const(Sum(1))

    lazy val cciBody = count

    lazy val cci: String => Count[List[Nothing]] = List.traverse(cciBody)

    def testCci {
        val res = cci("hello").old.old
        expect(5)(res)
    }

    lazy val testInt: Bool => Sum[Int] = b => if (b) Sum(1) else Sum(0)

    lazy val lciBody: Char => Count[Nothing] = c => Const(testInt(c == '\n'))

    lazy val lci: String => Count[List[Nothing]] = List.traverse(lciBody)

    def testLci {
        val res = lci("hello\nworld\n!").old.old
        expect(2)(res)
    }

    type IsWithinWord[+a] = State[Bool, a]
    type WciApp[+a] = Comp[IsWithinWord, Count, a]

    lazy val wciBody: Char => WciApp[Nothing] = c => {
        val updateState: Char => Bool => (Count[Nothing], Bool) = c => w => {
            val s = Bool.not(Char.isSpace(c))
            ( Const(testInt(Bool.not(w) && s)), s )
        }
        Comp[IsWithinWord, Count, Nothing](State((b: Bool) => updateState(c)(b)))
    }

    lazy val wci: String => WciApp[List[Nothing]] = List.traverse(wciBody)

    lazy val runWci: String => Int = s => wci(s)._1.apply(False)._1.old.old

    def testWci {
        expect(false)(Char.isSpace('\n')) // wow
        val res = runWci("hello  world   ! ")
        expect(3)(res)
    }
}
