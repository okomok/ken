


// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class MonadToArrowTest extends org.scalatest.junit.JUnit3Suite {

    def liftA2[a[_, _], b, c, d, e](op: b => c => d)(f: a[e, b])(g: a[e, c])(implicit i: Arrow[a]): a[e, d] = {
        import Arrow._
        (f &&& g) >>> arr{case (b, c) => op(b)(c)}
    }

    /*sealed*/ abstract class Exp
    case class Var(x: String) extends Exp
    case class Add(l: Exp, r: Exp) extends Exp
    case class If(b: Exp, t: Exp, e: Exp) extends Exp

    /*sealed*/ abstract class Val
    case class Num(x: Int) extends Val
    case class Bl(b: Boolean) extends Val

    type Env = List[(String, Val)]
    def lookup[a, b](key: a)(xs: List[(a, b)]): b = Maybe.fromJust(List.lookup(key)(xs))

    def add(v1: Val)(v2: Val): Val = (v1, v2) match {
        case (Num(u), Num(v)) => Num(u+v)
    }

    object Monadic {
        import Monad._
        def eval[m[_]](exp: Exp)(env: Env)(implicit i: Monad[m]): m[Val] = exp match {
            case Var(s) => `return`(lookup(s)(env))
            case Add(e1, e2) => liftM2(add)(eval(e1)(env))(eval(e2)(env))
            case If(e1, e2, e3) => eval(e1)(env) >>= { case Bl(b) =>
                if (b) eval(e2)(env) else eval(e3)(env)
            }
        }
    }

    val theEnv = List(("x", Num(1)), ("y", Num(2)), ("z", Num(3)))

    def testMonadic {
        val Num(x) !:: Nil = Monadic.eval[List](Add(Add(Var("x"), Var("y")), Var("z")))(theEnv)
        expect(6)(x)

        val Just(Num(y)) = Monadic.eval[Maybe](Add(Add(Var("x"), Var("y")), Var("z")))(theEnv)
        expect(6)(y)
    }

    object ArrowWay {
        import Arrow._
        def eval[a[_, _]](exp: Exp)(implicit i: ArrowChoice[a]): a[Env, Val] = exp match {
            case Var(s) => Arrow.arr(lookup(s))
            case Add(e1, e2) => liftA2(add)(eval(e1))(eval(e2))
            case If(e1, e2, e3) => {
                (eval(e1) &&& arr(id[Env])) >>>
                arr{ case (Bl(b), env) => if (b) Left(env).asEither else Right(env).asEither } >>>
                (eval(e2) ||| eval(e3))
            }
        }
    }
}
