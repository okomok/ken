


// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest


import com.github.okomok.ken


class SybTest extends org.scalatest.junit.JUnit3Suite {

    case class Company(depts: List[Dept])
    case class Dept(name: Name, manager: Manager, subunits: List[SubUnit])
    sealed abstract class SubUnit
    case class PersonUnit(employee: Employee) extends SubUnit
    case class DeptUnit(dept: Dept) extends SubUnit
    case class Employee(person: Person, salary: Salary)
    case class Person(name: Name, address: Address)
    case class Salary(salary: Float)
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

    def inc[a](k: Float)(x: a)(implicit ac: ken.Typeable[a]): a = ken.Typeable.mkT(incS(k))(x)

    def testInc {
        expect("hello")(inc(10)("hello"))
        expect(Salary(1050F))(inc(.05F)(Salary(1000)))
    }

    trait PolyTermFunc {
        def apply[b](x: b)(implicit i: Term[b]): b
    }

    trait Term[a] extends ken.Typeable[a] {
        def gmapT(f: PolyTermFunc)(x: a): a
    }

    object Term {
        implicit val instanceOfEmpolyee: Term[Employee] = new Term[Employee] {
            override def typeOf(x: => Employee) = implicitly[ClassManifest[Employee]]
            override def gmapT(f: PolyTermFunc)(x: Employee) = x match {
                case Employee(per, sal) => Employee(f(per), f(sal))
            }
        }

        implicit val instanceOfCompany: Term[Company] = new Term[Company] {
            override def typeOf(x: => Company) = implicitly[ClassManifest[Company]]
            override def gmapT(f: PolyTermFunc)(x: Company) = x match {
                case Company(x1) => Company(f(x1))
            }
        }

        implicit val instanceOfDept: Term[Dept] = new Term[Dept] {
            override def typeOf(x: => Dept) = implicitly[ClassManifest[Dept]]
            override def gmapT(f: PolyTermFunc)(x: Dept) = x match {
                case Dept(x1, x2, x3) => Dept(f(x1), f(x2), f(x3))
            }
        }
        implicit val instanceOfSubUnit: Term[SubUnit] = new Term[SubUnit] {
            override def typeOf(x: => SubUnit) = implicitly[ClassManifest[SubUnit]]
            override def gmapT(f: PolyTermFunc)(x: SubUnit) = x match {
                case PersonUnit(x1) => PersonUnit(f(x1))
                case DeptUnit(x1) => DeptUnit(f(x1))
            }
        }

        implicit val instanceOfPerson: Term[Person] = new Term[Person] {
            override def typeOf(x: => Person) = implicitly[ClassManifest[Person]]
            override def gmapT(f: PolyTermFunc)(x: Person) = x match {
                case Person(x1, x2) => Person(f(x1), f(x2))
            }
        }

        implicit val instanceOfSalary: Term[Salary] = new Term[Salary] {
            override def typeOf(x: => Salary) = implicitly[ClassManifest[Salary]]
            override def gmapT(f: PolyTermFunc)(x: Salary) = x match {
                case Salary(x1) => Salary(f(x1))
            }
        }

        implicit val instanceOfString: Term[String] = new Term[String] {
            override def typeOf(x: => String) = implicitly[ClassManifest[String]]
            override def gmapT(f: PolyTermFunc)(x: String) = x
        }

        implicit val instanceOfFloat: Term[Float] = new Term[Float] {
            override def typeOf(x: => Float) = implicitly[ClassManifest[Float]]
            override def gmapT(f: PolyTermFunc)(x: Float) = x
        }

        implicit def instanceOfList[a](implicit i: Term[a], k: ClassManifest[List[a]]): Term[List[a]] = new Term[List[a]] {
            //private[this] implicit val instance = this
            override def typeOf(x: => List[a]) = k
            override def gmapT(f: PolyTermFunc)(x: List[a]) = x match {
                case Nil => Nil
                case x :: xs => f(x) :: f(xs)
            }
        }
    }

    def everywhere[a](f: PolyTermFunc)(x: a)(implicit i: Term[a]): a = {
        class Everywhere(f: PolyTermFunc) extends PolyTermFunc {
            override def apply[b](x: b)(implicit i: Term[b]): b = f(i.gmapT(new Everywhere(f))(x))
        }
        new Everywhere(f).apply(x)
    }

    def everywhere_[a](f: PolyTermFunc)(x: a)(implicit i: Term[a]): a = {
        class Everywhere(f: PolyTermFunc) extends PolyTermFunc {
            override def apply[b](x: b)(implicit i: Term[b]): b = i.gmapT(new Everywhere(f))(f(x))
        }
        new Everywhere(f).apply(x)
    }

    def increase(k: Float)(c: Company): Company = {
        everywhere(
            new PolyTermFunc {
                override def apply[b](x: b)(implicit i: Term[b]): b = inc(k)(x)
            }
        )(c)
    }

    def testIncrease {
        println(genCom)
        println(increase(.05F)(genCom))
    }

}
