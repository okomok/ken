

// Public domain


package com.github.okomok.kentest.example


import com.github.okomok.ken._


// Scrap your boilerplate: a practical approach to generic programming
// http://research.microsoft.com/en-us/um/people/simonpj/papers/hmap/


class ScrapYourBoilerplateTest extends org.scalatest.junit.JUnit3Suite {

    // 2. The problem

    final case class Company(val _1: List[Dept])

    object Company extends Data[Company] with TypeableProxy[Company] with ThisIsInstance {
        private type a = Company
        override val selfTypeable = Typeable.of[a]
        override def gfoldl[c[_]](k: GenericL[c])(z: Pure[c])(a: a): c[a] = a match {
            case Company(v1) => k(z((w1: List[Dept]) => Company(w1)))(v1)
        }
    }

    final case class Dept(val _1: Name, val _2: Manager, val _3: List[SubUnit])

    object Dept extends Data[Dept] with TypeableProxy[Dept] with ThisIsInstance {
        private type a = Dept
        override val selfTypeable = Typeable.of[a]
        override def gfoldl[c[_]](k: GenericL[c])(z: Pure[c])(a: a): c[a] = a match {
            case Dept(v1, v2, v3) => k( k( k(z((w1: Name) => (w2: Manager) => (w3: List[SubUnit]) => Dept(w1, w2, w3)))(v1) )(v2) )(v3)
        }
    }

    sealed abstract class SubUnit extends Up[SubUnit]
    final case class PersonUnit(employee: Employee) extends SubUnit
    final case class DeptUnit(dept: Dept) extends SubUnit

    object SubUnit extends Data[SubUnit] with TypeableProxy[SubUnit] with ThisIsInstance {
        private type a = SubUnit
        override val selfTypeable = Typeable.of[a]
        override def gfoldl[c[_]](k: GenericL[c])(z: Pure[c])(a: a): c[a] = a match {
            case PersonUnit(e) => k(z(PersonUnit(_: Employee).up))(e)
            case DeptUnit(d) => k(z(DeptUnit(_: Dept).up))(d)
        }
    }

    final case class Employee(val _1: Person, val _2: Salary)

    object Employee extends Data[Employee] with TypeableProxy[Employee] with ThisIsInstance {
        private type a = Employee
        override val selfTypeable = Typeable.of[a]
        override def gfoldl[c[_]](k: GenericL[c])(z: Pure[c])(a: a): c[a] = a match {
            case Employee(p, s) => k( k(z((p: Person) => (s: Salary) => Employee(p, s)))(p) )(s)
        }
    }

    final case class Person(val _1: Name, val _2: Address)

    object Person extends Data[Person] with TypeableProxy[Person] with ThisIsInstance {
        private type a = Person
        override val selfTypeable = Typeable.of[a]
        override def gfoldl[c[_]](k: GenericL[c])(z: Pure[c])(a: a): c[a] = a match {
            case Person(p, s) => k( k(z((p: Name) => (s: Address) => Person(p, s)))(p) )(s)
        }
    }

    final case class Salary(val _1: Float)

    object Salary extends Data[Salary] with TypeableProxy[Salary] with ThisIsInstance {
        private type a = Salary
        override val selfTypeable = Typeable.of[a]
        override def gfoldl[c[_]](k: GenericL[c])(z: Pure[c])(a: a): c[a] = a match {
            case Salary(v1) => k(z((w1: Float) => Salary(w1)))(v1)
        }
    }

    type Manager = Employee
    type Name = String
    type Address = String


    lazy val genCom: Company = Company(
        List(
            Dept("Research", ralf, List(PersonUnit(joost), PersonUnit(marlow))),
            Dept("Strategy", blair, Nil)
        ) )

    val ralf: Employee = Employee(Person("Ralf", "Amsterdam"), Salary(8000))
    val joost: Employee = Employee(Person("Joost", "Amsterdam"), Salary(1000))
    val marlow: Employee = Employee(Person("Marlow", "Cambridge"), Salary(2000))
    val blair: Employee = Employee(Person("Blair", "London"), Salary(100000))

    val incS: Float => Salary => Salary = k => {
        case Salary(s) => Salary(s * (1+k))
    }

    // 3. Our solution

    def inc[a](k: Float)(x: a)(implicit ac: Typeable[a]): a = Typeable.mkT(incS(k))(x)

    def testInc {
        expect("hello")(inc(10)("hello"))
        expect(Salary(1050F))(inc(.05F)(Salary(1000)))
    }

    // 3.3 Recursive traversal

    def everywhere[a](f: GenericT)(x: a)(implicit i: Data[a]): a = {
        class Everywhere(f: GenericT) extends GenericT {
            override def apply[b](x: b)(implicit i: Data[b]): b = f(i.gmapT(new Everywhere(f))(x))
        }
        (new Everywhere(f))(x)
    }

    def everywhere_[a](f: GenericT)(x: a)(implicit i: Data[a]): a = {
        class Everywhere(f: GenericT) extends GenericT {
            override def apply[b](x: b)(implicit i: Data[b]): b = i.gmapT(new Everywhere(f))(f(x))
        }
        (new Everywhere(f))(x)
    }

    val increase: Float => Company => Company = k => c => {
        val f = new GenericT {
            override def apply[b](x: b)(implicit i: Data[b]): b = inc(k)(x)
        }
        everywhere(f)(c)
    }

    def testIncrease {
        val src = genCom
        val dst = increase(.05F)(genCom)
        assert(src != dst)
        //println(src)
        //println(dst)
    }

    // 3.4 Another example

    val flatD: Name => Dept => Dept = d => {
        case Dept(n, m, us) => {
            val unwrap: SubUnit => List[SubUnit] = {
                case DeptUnit(Dept(d_, m, us)) if d == d_ => PersonUnit(m) :: us
                case u => List(u)
            }
            Dept(n, m, List.concatMap(unwrap)(us))
        }
    }

    val flatten: Name => Company => Company = d => c => {
        val f = new GenericT {
            override def apply[b](b: b)(implicit i: Data[b]): b = Typeable.mkT(flatD(d))(b)
        }
        everywhere(f)(c)
    }

    // 4.1 Implementing queries

    val billS: Salary => Float = { case Salary(f) => f }

    def testBillS {
        expect(97)(Typeable.mkQ(22)(Char.ord)('a'))
        expect(98)(Typeable.mkQ(22)(Char.ord)('b'))
        expect(22)(Typeable.mkQ(22)(Char.ord)(True))
    }

    def everything[a, r](k: r => r => r)(f: GenericQ[r])(x: a)(implicit i: Data[a]): r = {
        class Everything(k: r => r => r, f: GenericQ[r]) extends GenericQ[r] {
            override def apply[d](x: d)(implicit i: Data[d]): r = {
                List.foldl(k)(f(x))(i.gmapQ(new Everything(k, f))(x))
            }
        }
        (new Everything(k, f))(x)
    }

    val salaryBill: Company => Float = c => {
        val f = new GenericQ[Float] {
            override def apply[d](x: d)(implicit i: Data[d]): Float = Typeable.mkQ(0F)(billS)(x)
        }
        everything((x: Float) => (y: Float) => x + y)(f)(c)
    }

    def testSalaryBill {
        expect(111000F)(salaryBill(genCom))
    }

    // 4.2 Other queries

    val findD: String => Dept => Maybe[Dept] = n => {
        case d @ Dept(n_, _, _) => if (n == n_) Just(d) else Nothing
    }

    val find: Name => Company => Maybe[Dept] = n => c => {
        val f = new GenericQ[Maybe[Dept]] {
            override def apply[d](x: d)(implicit i: Data[d]): Maybe[Dept] = Typeable.mkQ(Nothing.of[Dept])(findD(n))(x)
        }
        everything(Maybe.orElse[Dept])(f)(c)
    }

    def testFind {
        val Just(x) = find("Research")(genCom)
        expect(List.from("Research"))(x._1)
        expect(Nothing)(find("Wooo")(genCom))
    }

    // 5 Monadic transformation

    def everywhereM[a, m[+_]](f: GenericM[m])(x: a)(implicit i: Data[a], j: Monad[m]): m[a] = {
        class EverywhereM(f: GenericM[m]) extends GenericM[m] {
            override def apply[b](x: b)(implicit i: Data[b]): m[b] = {
                import j.`for`
                for {
                    x_ <- i.gmapM(new EverywhereM(f))(x)
                } {
                    f(x_)
                }
            }
        }
        (new EverywhereM(f))(x)
    }

    val lookupE: Employee => IO[Employee] = {
        case Employee(p @ Person(n, _), _) => for {
            s <- dbLookup(n)
        } yield Employee(p, s)
    }

    val dbLookup: Name => IO[Salary] = n => n.asJString match {
        case "Ralf" => IO.`return`(Salary(8005))
        case "Joost" => IO.`return`(Salary(1005))
        case "Marlow" => IO.`return`(Salary(2005))
        case "Blair" => IO.`return`(Salary(100005))
    }

    val lookupSalaries: Company => IO[Company] = c => {
        val f = new GenericM[IO] {
            override def apply[b](x: b)(implicit i: Data[b]): IO[b] = Typeable.mkM(lookupE)(x)
        }
        everywhereM(f)(c)
    }

    def testLookupSalaries {
        val src = genCom
        val dst = lookupSalaries(genCom).!
        assert(src != dst)
    }

    // 6.2 Richer traversals

    def gincrease[a](k: Float)(a: a)(implicit i: Data[a]): a = {
        val f = new GenericT {
            override def apply[b](x: b)(implicit i: Data[b]): b = inc(k)(x)
        }
        everywhere(f)(a)
    }

    val incrOne: Name => Float => GenericT = n => k => new GenericT {
        override def apply[a](a: a)(implicit i: Data[a]): a = {
            if (isDept(n)(a)) gincrease(k)(a)
            else i.gmapT(incrOne(n)(k))(a)
        }
    }

    val isDept: Name => GenericQ[Bool] = n => new GenericQ[Bool] {
        override def apply[b](b: b)(implicit i: Data[b]): Bool = Typeable.mkQ(False)(isDeptD(n))(b)
    }

    val isDeptD: Name => Dept => Bool = n => {
        case Dept(n_, _, _) => n == n_
    }

    def testIncrOne {
        val src = genCom
        val dst = Data.gmapT(incrOne("Strategy")(.05F))(src)
        assert(src != dst)
    }

    // 6.3 Identifying the interesting case

    // uninteresting!

}
