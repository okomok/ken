// Public domain

package com.github.okomok.kentest.example

import com.github.okomok.ken._

class OpenRecursionSumTypeTest extends org.scalatest.junit.JUnit3Suite {

    lazy val theEnv: List[(String, Int)] = error("todo")

    object Closed {
        sealed abstract class Expr
        final case class Var(_1: String) extends Expr
        final case class Add(_1: Expr, _2: Expr) extends Expr // "closed" recursion: `Expr` is fixed early.

        lazy val eval: Expr => Int = {
            case Var(s) => List.lookup(s)(theEnv) match {
                case Nothing => error("not found")
                case Just(n) => n
            }
            case Add(e1, e2) => {
                eval(e1) + eval(e2)
            }
        }
    }

    object Open {
        sealed abstract class Or[+f[+_], +g[+_], +e]
        final case class Left[f[+_], e](_1: f[e]) extends Or[f, Kind.Nothing1, e]
        final case class Right[g[+_], e](_1: g[e]) extends Or[Kind.Nothing1, g, e]

        final case class Fix[f[+_]](out: f[Fix[f]])

        final case class Var[+e](_1: String)
        final case class Add[+e](_1: e, _2: e) // open recursion

        type AddOrVar[+e] = Or[Add, Var, e]
        type Expr = Fix[AddOrVar]

        lazy val eval: Expr => Int = e => {
            e.out match {
                case Left(e) => eval(e._1) + eval(e._2)
                case Right(e) => List.lookup(e._1)(theEnv) match {
                    case Nothing => error("not found")
                    case Just(n) => n
                }
            }
        }
    }

    def testTrivial {
    }
}
