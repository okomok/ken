

// Public domain


package com.github.okomok.kentest.example


import com.github.okomok.ken._


// See: http://d.hatena.ne.jp/Lost_dog/20111128/1322504143
//      https://groups.google.com/group/haskell-cafe/browse_thread/thread/8bfc1cb622d1dd6c


class TaggedString1Test extends org.scalatest.junit.JUnit3Suite {

    type Stuff = List[Int]

    final case class IString[+id](rep: String)

    final case class Exists[f[+_]](rep: (f[x], x) forSome { type x })

    lazy val read: String => Exists[IString] = stuff => Exists(IString(stuff), stuff)

    trait Equal[a, b] {
        // def Refl: Equal[a, a]
        def apply[f[_], b](b: f[b]): f[a]
    }

    def equal[s, t](x: IString[s])(y: IString[t]): Maybe[Equal[s, t]] = (x, y) match {
        case (IString(x), IString(y)) if x == y => Just( new Equal[s, t] {
            override def apply[f[_], t](t: f[t]): f[s] = t.asInstanceOf[f[s]]
        })
        case _ => Nothing
    }

    final case class Thing[+id](id: IString[id], stuff: Stuff)

    object Thing {
        implicit def _asMonoid[id]: Monoid[Thing[id]] = new Monoid[Thing[id]] {
            override val mempty: mempty = Thing(null, Nil)
            override val mappend: mappend = { case Thing(id1, stuff1) => { case !(Thing(id2, stuff2)) =>
                Thing(id1, Monoid.mappend(stuff1)(stuff2))
            } }
        }
    }

    lazy val thing: String => Stuff => Exists[Thing] = id => stuff => {
        def impl[id_](rep: (IString[id_], id_)): Exists[Thing] = Exists(Thing(rep._1, stuff), rep._2)
        impl(read(id).rep)
    }

    lazy val maybeAppend: Exists[Thing] => Exists[Thing] => Maybe[Exists[Thing]] = t1 => t2 => {
        def impl[id1, id2](rep1: (Thing[id1], id1))(rep2: (Thing[id2], id2)): Maybe[Exists[Thing]] = {
            val t1: Thing[id1] = rep1._1
            val t2: Thing[id2] = rep2._1
            equal(t1.id)(t2.id) match {
                case Just(f) => Just { Exists((Monoid.mappend(t1)(f(t2)), rep1._2)) }
                case Nothing => Nothing
            }
        }
        impl(t1.rep)(t2.rep)
    }

    def testTrivial {
    }
}


class TaggedString2Test extends org.scalatest.junit.JUnit3Suite {

    type Stuff = List[Int]

    lazy val str_to_int: String => Integer = str => str.asJString.toInt
    lazy val int_to_str: Integer => String = n => n.toString

    // IDeal
    //
    trait IDeal[id] {
        def get_id: String
    }

    trait IDealQ[w] { // cf. GenericQ
        def apply[id](id: id)(implicit _I: IDeal[id]): w
    }

    // Thing
    //
    final case class Thing[+id](stuff: Stuff)
    object Thing {
        implicit def _asMonoid[id]: Monoid[Thing[id]] = new Monoid[Thing[id]] {
            override val mempty: mempty = Thing(Nil)
            override val mappend: mappend = { case Thing(t1) => { case !(Thing(t2)) =>
                Thing(Monoid.mappend(t1)(t2))
            } }
        }
    }

    trait ThingQ[w] {
        def apply[id](t: Thing[id])(implicit _I: IDeal[id]): w
    }

    def make_thing[id](id: id)(t: Stuff)(implicit _I: IDeal[id]): Thing[id] = Thing(t)

    def eqid_check[id1, id2](t1: Thing[id1])(t2: Thing[id2])(implicit _I1: IDeal[id1], _I2: IDeal[id2]): Maybe[Thing[id1]] = {
        if (_I2.get_id == _I1.get_id) Just(t2.asInstanceOf[Thing[id1]])
        else Nothing
    }

    // Id1
    //
    final class Id1
    object Id1 {
        implicit val _asIDeal: IDeal[Id1] = new IDeal[Id1] {
            override val get_id = List.from("Id1")
        }
    }

    // DynId
    //
    final class DynId[id]
    object DynId {
        implicit def _asIDeal[id](implicit _N: Nat[id]): IDeal[DynId[id]] = new IDeal[DynId[id]] {
            override val get_id = int_to_str { _N.nat }
        }
    }

    // Nat
    //
    final class Z
    final val Z = new Z
    final case class S[a](_1: a)
    final case class T[a](_1: a)

    trait Nat[a] {
        def nat: Integer
    }
    object Nat {
        implicit val _ofZ: Nat[Z] = new Nat[Z] {
            override val nat: Integer = 0
        }
        implicit def _ofS[a](implicit _N: Nat[a]): Nat[S[a]] = new Nat[S[a]] {
            override val nat = _N.nat + 1
        }
        implicit def _ofT[a](implicit _N: Nat[a]): Nat[T[a]] = new Nat[T[a]] {
            override val nat = _N.nat * 2
        }
    }

    trait NatQ[w] {
        def apply[id](n: id)(implicit _N: Nat[id]): w
    }

    import Integral._

    def reify_integer[w](n: Integer)(k: NatQ[w]): w = n match {
        case n if n == 0 => k(Z)
        case n if even(n) => reify_integer(n _div_ 2) {
            new NatQ[w] {
                override def apply[id](x: id)(implicit _N: Nat[id]): w = k(T(x))
            }
        }
        case n => reify_integer(n - 1) {
            new NatQ[w] {
                override def apply[id](x: id)(implicit _N: Nat[id]): w = k(S(x))
            }
        }
    }

    def read_thing[w](str: String)(k: ThingQ[w]): w = {
        val (i, t) = read(str)
        reify_ideal(i) {
            new IDealQ[w] {
                override def apply[id](iD: id)(implicit _I: IDeal[id]): w = k(make_thing(iD)(t)(_I))(_I)
            }
        }
    }

    def reify_ideal[w](str: String)(k: IDealQ[w]): w = {
        def dynid[id](id: id): DynId[id] = null
        reify_integer(str_to_int(str)) {
            new NatQ[w] {
                override def apply[id](n: id)(implicit _N: Nat[id]): w = k(dynid(n)) // (implicitly[IDeal[DynId[id]]])
            }
        }
    }

    lazy val tezt: String => String => String = file1 => file2 => {
        def do_thing[id1, id2](t1: Thing[id1])(t2: Thing[id2])(implicit _I1: IDeal[id1], _I2: IDeal[id2]): String = eqid_check(t1)(t2)(_I1, _I2) match {
            case Just(t2_) => do_thing_(t1)(t2_)
            case _ => error("bad things")
        }
        def do_thing_[id](t1: Thing[id])(t2: Thing[id]): String = show(Monoid.mappend(t1)(t2))

        read_thing(file1) {
            new ThingQ[String] {
                override def apply[id1](t1: Thing[id1])(implicit _I1: IDeal[id1]): String = read_thing(file2) {
                    new ThingQ[String] {
                        override def apply[id2](t2: Thing[id2])(implicit _I2: IDeal[id2]): String = do_thing(t1)(t2)(_I1, _I2)
                    }
                }
            }
        }
    }

    lazy val read: String => (String, List[Int]) = str => str.asJString match {
        case "data1" => ("123", List(1, 2))
        case "data2" => ("345", List(3, 4))
        case "data3" => ("123", List(5, 6))
    }

    def testTrivial {
        expect("Thing(List(1,2,5,6))") {
            tezt("data1")("data3").asJString
        }

        intercept[ErrorError] {
            tezt("data1")("data2")
        }
    }
}
