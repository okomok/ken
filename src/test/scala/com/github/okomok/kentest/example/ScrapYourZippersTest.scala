

// Public domain


package com.github.okomok.kentest.example


import com.github.okomok.ken._


// Scrap Your Zippers: A Generic Zipper for Heterogeneous Types
// https://www.cs.indiana.edu/~adamsmd/papers/scrap_your_zippers/


import scala.annotation.tailrec


class ScrapYourZippersTest extends org.scalatest.junit.JUnit3Suite {

    // 2.1 Implementing the Zipper

    sealed abstract class Term
    final case class Var(_1: String) extends Term
    final case class Lambda(_1: String, _2: Term) extends Term
    final case class App(_1: Term, _2: Term) extends Term
    final case class If(_1: Term, _2: Term, _3: Term) extends Term

    type TermZipper = (Term, TermContext)

    sealed abstract class TermContext
    case object TermRoot extends TermContext
    final case class Lambda_1(_1: String, _2: TermContext) extends TermContext
    final case class App_1(_1: TermContext, _2: Term) extends TermContext
    final case class App_2(_1: Term, _2: TermContext) extends TermContext
    final case class If_1(_1: TermContext, _2: Term, _3: Term) extends TermContext
    final case class If_2(_1: Term, _2: TermContext, _3: Term) extends TermContext
    final case class If_3(_1: Term, _2: Term, _3: TermContext) extends TermContext

    val term_down: TermZipper => Maybe[TermZipper] = {
        case (Var(s), c) => Nothing
        case (Lambda(s, t1), c) => Just(t1, Lambda_1(s, c))
        case (App(t1, t2), c) => Just(t1, App_1(c, t2))
        case (If(t1, t2, t3), c) => Just(t1, If_1(c, t2, t3))
    }

    val term_up: TermZipper => Maybe[TermZipper] = {
        case (t1, TermRoot) => Nothing
        case (t1, Lambda_1(s, c)) => Just(Lambda(s, t1), c)
        case (t1, App_1(c, t2)) => Just(App(t1, t2), c)
        case (t2, App_2(t1, c)) => Just(App(t1, t2), c)
        case (t1, If_1(c, t2, t3)) => Just(If(t1, t2, t3), c)
        case (t2, If_2(t1, c, t3)) => Just(If(t1, t2, t3), c)
        case (t3, If_3(t1, t2, c)) => Just(If(t1, t2, t3), c)
    }

    val term_left: TermZipper => Maybe[TermZipper] = {
        case (t1, TermRoot) => Nothing
        case (t1, Lambda_1(s, c)) => Nothing
        case (t1, App_1(c, t2)) => Nothing
        case (t2, App_2(t1, c)) => Just(t1, App_1(c, t2))
        case (t1, If_1(c, t2, t3)) => Nothing
        case (t2, If_2(t1, c, t3)) => Just(t1, If_1(c, t2, t3))
        case (t3, If_3(t1, t2, c)) => Just(t2, If_2(t1, c, t3))
    }

    val term_right: TermZipper => Maybe[TermZipper] = {
        case (t1, TermRoot) => Nothing
        case (t1, Lambda_1(s, c)) => Nothing
        case (t1, App_1(c, t2)) => Just(t2, App_2(t1, c))
        case (t2, App_2(t1, c)) => Nothing
        case (t1, If_1(c, t2, t3)) => Just(t2, If_2(t1, c, t3))
        case (t2, If_2(t1, c, t3)) => Just(t3, If_3(t1, t2, c))
        case (t3, If_3(t1, t2, c)) => Nothing
    }

    val fromZipperTerm: TermZipper => Term = { z =>
        @tailrec
        def f(z: TermZipper): Term = z match {
            case (t1, TermRoot) => t1
            case (t1, Lambda_1(s, c)) => f(Lambda(s, t1), c)
            case (t1, App_1(c, t2)) => f(App(t1, t2), c)
            case (t2, App_2(t1, c)) => f(App(t1, t2), c)
            case (t1, If_1(c, t2, t3)) => f(If(t1, t2, t3), c)
            case (t2, If_2(t1, c, t3)) => f(If(t1, t2, t3), c)
            case (t3, If_3(t1, t2, c)) => f(If(t1, t2, t3), c)
        }
        f(z)
    }

    val term_toZipper: Term => TermZipper = t => (t, TermRoot)
    val term_getHole: TermZipper => Term = { case (t, _) => t }
    val term_setHole: Term => TermZipper => TermZipper = h => { case (_, c) => (h, c) }

    // 2.2 Using the Zipper

    val fac = Lambda("n",
        If( App(App(Var("="), Var("n")), Var("0")),
            Var("1"),
            App(App(Var("+"), Var("n")),
                App(Var("fac"),
                    App(Var("pred"), Var("n"))))))

    def test2_2 {
        val t0 = term_toZipper(fac)
        expect(fac)(term_getHole(t0))

        val Just(t1) = term_down(t0)
        expect(fac._2)(term_getHole(t1))

        val Just(t2) = term_down(t1)
        expect( App(App(Var("="), Var("n")), Var("0")) )(term_getHole(t2))

        val Just(t3) = term_right(t2)
        expect( Var("1") )(term_getHole(t3))

        val Just(t4) = term_right(t3)
        expect( App(App(Var("+"), Var("n")),
                App(Var("fac"),
                    App(Var("pred"), Var("n")))) )(term_getHole(t4))

        val Just(t5) = term_down(t4)
        expect( App(Var("+"), Var("n")) )(term_getHole(t5))

        val Just(t6) = term_down(t5)
        expect( Var("+") )(term_getHole(t6))

        val t7 = term_setHole(Var("*"))(t6)
        expect( Var("*") )(term_getHole(t7))

        val Just(t8) = term_up(t7)
        expect( App(Var("*"), Var("n")) )(term_getHole(t8))

        val Just(t9) = term_up(t8)
        val Just(t10) = term_up(t9)
        val Just(t11) = term_up(t10)

        val answer = Lambda("n",
        If( App(App(Var("="), Var("n")), Var("0")),
            Var("1"),
            App(App(Var("*"), Var("n")),
                App(Var("fac"),
                    App(Var("pred"), Var("n"))))))

        expect(answer)(term_getHole(t11))
        expect(answer)(fromZipperTerm(t7))
    }

    // 3. Using the Generic Zipper

    final case class Dept(_1: Manager, _2: List[Employee])

    object Dept extends Data[Dept] with TypeableProxy[Dept] with ThisIsInstance {
        private type a = Dept
        override val selfTypeable = Typeable.of[a]
        override def gfoldl[c[_]](k: GenericL[c])(z: Pure[c])(a: a): c[a] = a match {
            case Dept(v1, v2) => k( k(z((w1: Manager) => (w2: List[Employee]) => Dept(w1, w2)))(v1) )(v2)
        }
    }

    final case class Employee(_1: Name, _2: Salary)

    object Employee extends Data[Employee] with TypeableProxy[Employee] with ThisIsInstance {
        private type a = Employee
        override val selfTypeable = Typeable.of[a]
        override def gfoldl[c[_]](k: GenericL[c])(z: Pure[c])(a: a): c[a] = a match {
            case Employee(v1, v2) => k( k(z((w1: Name) => (w2: Salary) => Employee(w1, w2)))(v1) )(v2)
        }
    }

    type Salary = Float
    type Manager = Employee
    type Name = String

    val agamemnon: Employee = Employee("Agamemnon", 5000F)
    val menelaus: Employee = Employee("Menelaus", 3000F)
    val achilles: Employee = Employee("Achilles", 2000F)
    val odysseus: Employee = Employee("Odysseus", 2000F)

    val dept: Dept = Dept(agamemnon, List(menelaus, achilles, odysseus))


    // Hole manipulations

    def query[a, b](f: GenericQ[b])(z: Zipper[a]): b = {
        def impl[hole](rep: ZipperRep[a, hole]): b = {
            //println("query: " + rep._1 + " with " + rep._3.typeOf(0))
            f(rep._1)(rep._3)
        }
        impl(z.rep)
    }

    def trans[a](f: GenericT)(z: Zipper[a]): Zipper[a] = {
        def impl[hole](rep: ZipperRep[a, hole]): Zipper[a] = Zipper(f(rep._1)(rep._3), rep._2)(rep._3)
        impl(z.rep)
    }

    def transM[a, m[+_]](f: GenericM[m])(z: Zipper[a])(implicit i: Monad[m]): m[Zipper[a]] = {
        def impl[hole](rep: ZipperRep[a, hole]): m[Zipper[a]] = {
            import i.`for`
            for {
                hole_ <- f(rep._1)(rep._3)
            } yield Zipper(hole_, rep._2)(rep._3)
        }
        impl(z.rep)
    }

    // Retrieves a hole if its type is `b`.
    def getHole[a, b](z: Zipper[a], * : Type[b] = null)(implicit i: Typeable[b]): Maybe[b] = {
        query(
            new GenericQ[Maybe[b]] {
                def apply[d](d: d)(implicit ii: Data[d]): Maybe[b] = {
                    //println("cast: "+ d + " by " + ii.typeOf(1) + " to " + i.typeOf(0))
                    Typeable.cast(d, Type[b])(ii, i)
                }
            }
        )(z)
    }

    // Set a hole value if its type is `a`; otherwise unchanged.
    def setHole[a, b](h: a)(z: Zipper[b])(implicit i: Typeable[a]): Zipper[b] = {
        trans(
            new GenericT {
                override def apply[d](d: d)(implicit ii: Data[d]): d = Typeable.mkT(const(h))(d)
            }
        )(z)
    }

    def test3 {
        val g1: Zipper[Dept] = toZipper(dept)(Dept)
        val Just(g2) = down(g1)

        expect(Just(dept._2))(getHole(g2, Type[List[Employee]]))

        val Just(g3) = left(g2)
        expect(Just(agamemnon))(getHole(g3, Type[Employee]))

        val Just(g4) = down(g3)
        expect(Just(5000F))(getHole(g4, Type[Salary]))

        val Just(g5) = left(g4)
        expect(Just(List.from("Agamemnon")))(getHole(g5, Type[Name]))

        val g6 = setHole(List.from("King Agamemnon"))(g5)

        val Just(g7) = right(g6)
        val g8 = setHole(8000.0F)(g7)

        val Just(g9) = up(g8)
        expect(Just(Employee("King Agamemnon", 8000.0F)))(getHole(g9, Type[Employee]))
    }

    // 4.1 Traversal Helpers

    type Move[a] = Zipper[a] => Maybe[Zipper[a]]
    type Query[a, b] = Zipper[a] => b
    type Trans[a] = Zipper[a] => Zipper[a] // Query[a, Zipper[a]]

    def moveQ[a, b](move: Move[a])(b: b)(f: Query[a, b]): Query[a, b] = z => move(z) match {
        case Nothing => b
        case Just(z_) => f(z_)
    }

    def leftQ[a, b](b: b)(f: Query[a, b]): Query[a, b] = moveQ(left[a])(b)(f)
    def rightQ[a, b](b: b)(f: Query[a, b]): Query[a, b] = moveQ(right[a])(b)(f)
    def downQ[a, b](b: b)(f: Query[a, b]): Query[a, b] = moveQ(down[a])(b)(f)
    def upQ[a, b](b: b)(f: Query[a, b]): Query[a, b] = moveQ(up[a])(b)(f)

    def moveT[a, b](move1: Move[a])(move2: Move[a])(b: Zipper[a])(f: Trans[a]): Trans[a] = z => {
        moveQ(move1)(b)(moveQ(move2)(b)(id) `.` f)(z)
    }

    def leftT[a](f: Trans[a]): Trans[a] = z => moveT(left[a])(right[a])(z)(f)(z)
    def rightT[a](f: Trans[a]): Trans[a] = z => moveT(right[a])(left[a])(z)(f)(z)
    def downT[a](f: Trans[a]): Trans[a] = z => moveT(down[a])(up[a])(z)(f)(z)
    def upT[a](f: Trans[a]): Trans[a] = z => {
        lazy val g: Zipper[a] => Zipper[a] = z_ => moveT(right[a])(left[a])(h(z_))(g)(z_)
        lazy val h: Zipper[a] => Zipper[a] = z_ => moveT(up[a])(down[a])(z_)(f)(z_)
        g(z)
    }

    def leftmost[a](z: Zipper[a]): Zipper[a] = leftQ[a, Zipper[a]](z)(leftmost[a])(z)
    def rightmost[a](z: Zipper[a]): Zipper[a] = rightQ[a, Zipper[a]](z)(rightmost[a])(z)

    def zeverywhere[a](f: GenericT)(z: Zipper[a]): Zipper[a] = {
        lazy val g: Trans[a] = z_ => leftT(g)(zeverywhere(f)(z_))
        trans(f)(downT(g)(z))
    }

    def zeverywhere_[a](f: GenericT)(z: Zipper[a]): Zipper[a] = {
        val x: Zipper[a] = trans(f)(z)
        lazy val g: Trans[a] = z_ => rightQ[a, Zipper[a]](upQ[a, Zipper[a]](z_)(g)(z_))(zeverywhere_(f))(z_)
        downQ[a, Zipper[a]](g(x))(zeverywhere_[a](f)_ `.` leftmost[a])(x)
    }

    def zreduce[a](f: GenericM[Maybe])(z: Zipper[a]): Zipper[a] = transM(f)(z) match {
        case Nothing => {
            lazy val g: Trans[a] = z_ => rightQ[a, Zipper[a]](upQ[a, Zipper[a]](z_)(g)(z_))(zreduce(f))(z_)
            downQ[a, Zipper[a]](g(z))(zreduce[a](f)_ `.` leftmost[a])(z)
        }
        case Just(x) => zreduce(f)(reduceAncestors(f)(x)(x))
    }

    def reduceAncestors[a](f: GenericM[Maybe])(z: Zipper[a])(_def: Zipper[a]): Zipper[a] = {
        lazy val g: Trans[a] = z_ => {
            lazy val _def_ : Zipper[a] = transM(f)(z_) match {
                case Nothing => _def
                case Just(x) => reduceAncestors(f)(x)(x)
            }
            reduceAncestors(f)(z_)(_def_)
        }
        upQ[a, Zipper[a]](_def)(g)(z)
    }

    // 5. Implementing the Generic Zipper

    // Zipper

    // See: SI-5022

    // Strong type needed for scalac matcher, which dislikes `forSome`.
    // Notice that `ZipperRep[a, _]` is not the same type as `ZipperRep[a, _]`
    final case class ZipperRep[+root, hole](_1: hole, _2: Context[hole, root], _3: Data[hole])
    final case class Zipper[+root](rep: ZipperRep[root, _])

    object Zipper {
        def apply[hole, root](h: hole, c: Context[hole, root])(implicit i: Data[hole]): Zipper[root] = {
            Predef.assert(i.typeOf(0).toString != "Any")
            new Zipper(new ZipperRep(h, c, i))
        }
    }

    // Context

    sealed abstract class Context[+hole, +root]
    case object CtxtNull extends Context[Nothing, Nothing]
    final case class CtxtConsRep[hole, root, rights, parent](_1: Left[hole => rights], _2: Right[rights, parent], _3: Context[parent, root], _4: Data[parent])
    final case class CtxtCons[hole, root](rep: CtxtConsRep[hole, root, _, _]) extends Context[hole, root]

    object CtxtCons {
        def apply[hole, root, rights, parent](l: Left[hole => rights], r: Right[rights, parent], c: Context[parent, root])(implicit i: Data[parent]): Context[hole, root] = {
            new CtxtCons(new CtxtConsRep(l, r, c, i))
        }
    }

    // Left

    sealed abstract class Left[+expects]
    final case class LeftUnit[expects](_1: expects) extends Left[expects]
    final case class LeftConsRep[expects, b](_1: Left[b => expects], _2: b, _3: Data[b])
    final case class LeftCons[expects](rep: LeftConsRep[expects, _]) extends Left[expects]

    object LeftCons {
        def apply[expects, b](l: Left[b => expects], b: b)(implicit i: Data[b]): Left[expects] = new LeftCons(new LeftConsRep(l, b, i))
    }

    // Right

    sealed abstract class Right[+provides, +parent]
    case object RightNull extends Right[Nothing, Nothing]
    final case class RightCons[a, b, t](_1: b, _2: Right[a, t], _3: Data[b]) extends Right[b => a, t]

    object RightCons {
        def apply[a, b, t](b: b, r: Right[a, t])(implicit i: Data[b]): Right[b => a, t] = new RightCons(b, r, i)
    }

    def combine[hole, rights, parent](lefts: Left[hole => rights])(hole: hole)(rights: Right[rights, parent]): parent = {
        fromRight(fromLeft(lefts)(hole))(rights)
    }

    final def fromLeft[r](l: Left[r]): r = l match {
        case LeftUnit(a) => a
        case LeftCons(rep: LeftConsRep[r, _]) => { // type-ascription for partial workaround of SI-5022.
            def impl[b](rep: LeftConsRep[r, b]): r = fromLeft(rep._1)(rep._2)
            impl(rep)
        }
    }

    final def fromRight[r, parent](f: r)(r: Right[r, parent]): parent = r match {
        case RightNull => f.asInstanceOf[parent]
        case RightCons(b, r, _) => fromRight(f.asInstanceOf[Any => Any](b))(r)
    }

    def toZipper[a](x: a)(implicit i: Data[a]): Zipper[a] = Zipper(x, CtxtNull)(i)
    def fromZipper[a](z: Zipper[a]): a = error("todo")

    def toLeft[a](a: a)(implicit i: Data[a]): Left[a] = {
        type c[d] = Left[d]
        val k = new GenericL[c] {
            override def apply[d, b](c: c[d => b])(x: d)(implicit ii: Data[d]): c[b] = LeftCons(c, x)(ii)
        }
        val z = new Pure[c] {
            override def apply[g](g: g): c[g] = LeftUnit(g)
        }
        i.gfoldl[c](k)(z)(a)
    }

    def up[a](z: Zipper[a]): Maybe[Zipper[a]] = z match {
        case Zipper(ZipperRep(_, CtxtNull, _)) => Nothing
        case Zipper(ZipperRep(hole, CtxtCons(CtxtConsRep(l, r, ctxt, j)), i)) => Just(Zipper(combine(l)(hole)(r)/*: parent*/, ctxt)(j))
    }

    def down[a](z: Zipper[a]): Maybe[Zipper[a]] = z match {
        case Zipper(ZipperRep(hole/*: hole*/, ctxt, i/*: Data[hole]*/)) => toLeft(hole)(i)/*: Left[hole]*/ match {
            case LeftUnit(_) => Nothing
            case LeftCons(LeftConsRep(l/*: Left[b => hole]*/, hole_/*: b*/, j/*: Data[b]*/)) => Just(Zipper(hole_, CtxtCons(l, RightNull, ctxt)(i))(j))
        }
    }

    def left[a](z: Zipper[a]): Maybe[Zipper[a]] = z match {
        case Zipper(ZipperRep( _, CtxtNull, _ )) => Nothing
        case Zipper(ZipperRep( _, CtxtCons(CtxtConsRep( LeftUnit(_), _, _, _ )), _ )) => Nothing
        case Zipper(ZipperRep( h/*: hole*/, CtxtCons(CtxtConsRep( LeftCons(LeftConsRep( l/*: b => hole => parents*/, h_/*: b*/, k/*: Data[b]*/ ))/*: Left[hole => parents]*/, r/*: Right[rights, parent]*/, c/*: Context[parent, root]*/, j/*: Data[parent]*/ ))/*: Context[hole, root]*/, i/*: Data[hole]*/ )) => {
            Just(  Zipper( h_, CtxtCons( l, RightCons(h, r)(i), c )(j) )(k)  )
        }
    }

    def right[a](z: Zipper[a]): Maybe[Zipper[a]] = z match {
        case Zipper(ZipperRep( _, CtxtNull, _ )) => Nothing
        case Zipper(ZipperRep( _, CtxtCons(CtxtConsRep( _, RightNull, _, _ )), _ )) => Nothing
        case Zipper(ZipperRep( h/*: hole*/, CtxtCons(CtxtConsRep( l/* Left[hole => parents]*/, RightCons(h_/*: b*/, r/*: Right[a, t]*/, k/*: Data[b]*/)/*: Right[rights, parent]*/, c/*: Context[parent, root]*/, j/*: Data[parent]*/ ))/*: Context[hole, root]*/, i/*: Data[hole]*/ )) => {
            Just(  Zipper( h_, CtxtCons( LeftCons(l, h)(i).asInstanceOf[Left[Any => Any]], r, c )(j) )(k)  )
        }
    }
}
