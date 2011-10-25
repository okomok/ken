

// Public domain


package com.github.okomok.kentest.example


import com.github.okomok.ken._


// Scrap your boilerplate: a practical approach to generic programming
// http://research.microsoft.com/en-us/um/people/simonpj/papers/hmap/


class ScrapYourBoilerplateTest extends org.scalatest.junit.JUnit3Suite {

    final case class Company(val _1: List[Dept])

    object Company extends Data[Company] with TypeableProxy[Company] with ThisIsInstance {
        private type a = Company
        override val selfTypeable = Typeable.of[a]
        override def gfoldl[c[_]](k: GApply[c])(z: GPure[c])(a: a): c[a] = a match {
            case Company(v1) => k(z((w1: List[Dept]) => Company(w1)))(v1)
        }
    }

    final case class Dept(val _1: Name, val _2: Manager, val _3: List[SubUnit])

    object Dept extends Data[Dept] with TypeableProxy[Dept] with ThisIsInstance {
        private type a = Dept
        override val selfTypeable = Typeable.of[a]
        override def gfoldl[c[_]](k: GApply[c])(z: GPure[c])(a: a): c[a] = a match {
            case Dept(v1, v2, v3) => k( k( k(z((w1: Name) => (w2: Manager) => (w3: List[SubUnit]) => Dept(w1, w2, w3)))(v1) )(v2) )(v3)
        }
    }

    sealed abstract class SubUnit extends Up[SubUnit]
    final case class PersonUnit(employee: Employee) extends SubUnit
    final case class DeptUnit(dept: Dept) extends SubUnit

    object SubUnit extends Data[SubUnit] with TypeableProxy[SubUnit] with ThisIsInstance {
        private type a = SubUnit
        override val selfTypeable = Typeable.of[a]
        override def gfoldl[c[_]](k: GApply[c])(z: GPure[c])(a: a): c[a] = a match {
            case PersonUnit(e) => k(z(PersonUnit(_: Employee).up))(e)
            case DeptUnit(d) => k(z(DeptUnit(_: Dept).up))(d)
        }
    }

    final case class Employee(val _1: Person, val _2: Salary)

    object Employee extends Data[Employee] with TypeableProxy[Employee] with ThisIsInstance {
        private type a = Employee
        override val selfTypeable = Typeable.of[a]
        override def gfoldl[c[_]](k: GApply[c])(z: GPure[c])(a: a): c[a] = a match {
            case Employee(p, s) => k( k(z((p: Person) => (s: Salary) => Employee(p, s)))(p) )(s)
        }
    }

    final case class Person(val _1: Name, val _2: Address)

    object Person extends Data[Person] with TypeableProxy[Person] with ThisIsInstance {
        private type a = Person
        override val selfTypeable = Typeable.of[a]
        override def gfoldl[c[_]](k: GApply[c])(z: GPure[c])(a: a): c[a] = a match {
            case Person(p, s) => k( k(z((p: Name) => (s: Address) => Person(p, s)))(p) )(s)
        }
    }

    final case class Salary(val _1: Float)

    object Salary extends Data[Salary] with TypeableProxy[Salary] with ThisIsInstance {
        private type a = Salary
        override val selfTypeable = Typeable.of[a]
        override def gfoldl[c[_]](k: GApply[c])(z: GPure[c])(a: a): c[a] = a match {
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

    def incS(k: Float)(s: Salary): Salary = s match {
        case Salary(s) => Salary(s * (1+k))
    }

    def inc[a](k: Float)(x: a)(implicit ac: Typeable[a]): a = Typeable.mkT(incS(k))(x)

    def testInc {
        expect("hello")(inc(10)("hello"))
        expect(Salary(1050F))(inc(.05F)(Salary(1000)))
    }

    def everywhere[a](f: Data.GId)(x: a)(implicit i: Data[a]): a = {
        class Everywhere(f: Data.GId) extends Data.GId {
            override def apply[b](x: b)(implicit i: Data[b]): b = f(i.gmapT(new Everywhere(f))(x))
        }
        (new Everywhere(f))(x)
    }

    def everywhere_[a](f: Data.GId)(x: a)(implicit i: Data[a]): a = {
        class Everywhere(f: Data.GId) extends Data.GId {
            override def apply[b](x: b)(implicit i: Data[b]): b = i.gmapT(new Everywhere(f))(f(x))
        }
        (new Everywhere(f))(x)
    }

    def increase(k: Float)(c: Company): Company = {
        everywhere(
            new Data.GId {
                override def apply[b](x: b)(implicit i: Data[b]): b = inc(k)(x)
            }
        )(c)
    }

    def testIncrease {
        val src = genCom
        val dst = increase(.05F)(genCom)
        expect(false)(src == dst)
        //println(src)
        //println(dst)
    }
}
