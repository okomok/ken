// Public domain

package com.github.okomok.kentest.example

import com.github.okomok.ken._

// http://www.grabmueller.de/martin/www/pub/Transformers.en.html
// http://debasishg.blogspot.com/2011/07/monad-transformers-in-scala.html

class MonadTransformersStepByStepTest extends org.scalatest.junit.JUnit3Suite {

    // 1.1 Example Program

    type Name = String

    sealed abstract class Exp
    final case class Lit(_1: Int) extends Exp
    final case class Var(_1: Name) extends Exp
    final case class Plus(_1: Exp, _2: Exp) extends Exp
    final case class Abs(_1: Name, _2: Exp) extends Exp
    final case class App(_1: Exp, _2: Exp) extends Exp

    sealed abstract class Value
    final case class IntVal(_1: Int) extends Value
    final case class FunVal(_1: Env, _2: Name, _3: Exp) extends Value

    type Env = Map[Name, Value]

    lazy val eval0: Env => Exp => Value = env => {
        case Lit(i) => IntVal(i)
        case Var(n) => env(n)
        case Plus(e1, e2) => {
            val IntVal(i1) = eval0(env)(e1)
            val IntVal(i2) = eval0(env)(e2)
            IntVal(i1 + i2)
        }
        case Abs(n, e) => FunVal(env, n, e)
        case App(e1, e2) => {
            val val1 = eval0(env)(e1)
            val val2 = eval0(env)(e2)
            val1 match {
                case FunVal(env_, n, body) => eval0(env_ + ((n, val2)))(body)
                case _ => error("match error")
            }
        }
    }

    lazy val exampleExp = Plus(Lit(12), App(Abs("x", Var("x")), Plus(Lit(4), Lit(2))))

    def testEval0 {
        val res = eval0(Map.empty)(exampleExp)
        expect(IntVal(18))(res)
    }

    // 2. Monad Transformers

    // 2.1 Converting to Monadic Style

    type Eval1[+a] = Identity[a]
    val Eval1 = Identity
    def runEval1[a](ev: Eval1[a]): a = ev.run

    lazy val eval1: Env => Exp => Eval1[Value] = env => {
        case Lit(i) => Eval1.`return` { IntVal(i) }
        case Var(n) => Eval1.`return` { env(n) }
        case Plus(e1, e2) => for {
            IntVal(i1) <- eval1(env)(e1)
            IntVal(i2) <- eval1(env)(e2)
        } Eval1.`return` { IntVal(i1 + i2) }
        case Abs(n, e) => Eval1.`return` { FunVal(env, n, e) }
        case App(e1, e2) => for {
            val1 <- eval1(env)(e1)
            val2 <- eval1(env)(e2)
        } {
            val1 match {
                case FunVal(env_, n, body) => eval1(env_ + ((n, val2)))(body)
                case _ => error("type error")
            }
        }
    }

    def testEval1 {
        val res = runEval1 { eval1(Map.empty)(exampleExp) }
        expect(IntVal(18))(res)
    }

    // 2.2 Adding Error Handling

    lazy val _M2 = MonadError[String, ErrorT.apply2[String, Identity.type]]
    type Eval2[+a] = _M2.apply[a]
    def runEval2[a](ev: Eval2[a]): Either[String, a] = ev.run.run

    lazy val eval2: Env => Exp => Eval2[Value] = {
        import _M2._
        env => {
            case Lit(i) => `return` { IntVal(i) }
            case Var(n) => `return` { env(n) }
            case Plus(e1, e2) => for {
                e1_ <- eval2(env)(e1)
                e2_ <- eval2(env)(e2)
            } {
                (e1_, e2_) match {
                    case (IntVal(i1), IntVal(i2)) => `return` { IntVal(i1 + i2) }
                    case _ => throwError("type error in addition")
                }
            }
            case Abs(n, e) => `return` { FunVal(env, n, e) }
            case App(e1, e2) => for {
                val1 <- eval2(env)(e1)
                val2 <- eval2(env)(e2)
            } {
                val1 match {
                    case FunVal(env_, n, body) => eval2(env_ + ((n, val2)))(body)
                    case _ => throwError("type error in application")
                }
            }
        }
    }

    def testEval2 {
        val res = runEval2 { eval2(Map.empty)(exampleExp) }
        expect(Right(IntVal(18)))(res)

        val res2 = runEval2 { eval2(Map.empty)(Plus(Lit(1), Abs("x", Var("x")))) }
        expect(Left(String("type error in addition")))(res2)
    }

    // 2.3 Hiding the Environment

    lazy val _M3 = MonadReader[Env, ReaderT.apply2[Env, _M2.type]]
    type Eval3[+a] = _M3.apply[a]
    def runEval3[a](env: Env)(ev: Eval3[a]): Either[String, a] = (ev.run)(env).run.run
    lazy val _M3E = MonadError[String, _M3.type]

    lazy val eval3: Exp => Eval3[Value] = exp => {
        import _M3._
        import _M3E.throwError
        exp match {
            case Lit(i) => `return` { IntVal(i) }
            case Var(n) => for {
                env <- ask
            } `return` { env(n) }
            case Plus(e1, e2) => for {
                e1_ <- eval3(e1)
                e2_ <- eval3(e2)
            } {
                (e1_, e2_) match {
                    case (IntVal(i1), IntVal(i2)) => `return` { IntVal(i1 + i2) }
                    case _ => throwError("type error in addition")
                }
            }
            case Abs(n, e) => for {
                env <- ask
            } `return` { FunVal(env, n, e) }
            case App(e1, e2) => for {
                val1 <- eval3(e1)
                val3 <- eval3(e2)
            } {
                val1 match {
                    case FunVal(env_, n, body) => {
                        local(const(env_ + ((n, val3))))(eval3(body))
                    }
                    case _ => throwError("type error in application")
                }
            }
        }
    }

    def testEval3 {
        val res = runEval3(Map.empty)(eval3(exampleExp))
        expect(Right(IntVal(18)))(res)
    }

    // 2.4 Adding State

    lazy val _M4 = MonadReader[Env, ReaderT.apply2[Env, ErrorT.apply2[String, StateT.apply2[Int, Identity.type]]]]
    type Eval4[+a] = _M4.apply[a]
    lazy val _M4E = MonadError[String, _M4.type]
    def runEval4[a](env: Env)(st: Int)(ev: Eval4[a]): (Either[String, a], Int) = ((ev.run)(env).run)(st).run

    def tick[s, m[+_]](implicit _N: Num[s], _MS: MonadState[s, m]): m[Unit] = {
        import _MS._
        import _N._
        for { st <- get } put(st + 1)
    }

    lazy val eval4: Exp => Eval4[Value] = exp => {
        import _M4._
        import _M4E.throwError
        exp match {
            case Lit(i) => for {
                _ <- tick[Int, Eval4]
            } `return` { IntVal(i) }
            case Var(n) => for {
                _ <- tick[Int, Eval4]
                env <- ask
            } `return` { env(n) }
            case Plus(e1, e2) => for {
                _ <- tick[Int, Eval4]
                e1_ <- eval4(e1)
                e2_ <- eval4(e2)
            } {
                (e1_, e2_) match {
                    case (IntVal(i1), IntVal(i2)) => `return` { IntVal(i1 + i2) }
                    case _ => throwError("type error in addition")
                }
            }
            case Abs(n, e) => for {
                _ <- tick[Int, Eval4]
                env <- ask
            } `return` { FunVal(env, n, e) }
            case App(e1, e2) => for {
                _ <- tick[Int, Eval4]
                val1 <- eval4(e1)
                val4 <- eval4(e2)
            } {
                val1 match {
                    case FunVal(env_, n, body) => {
                        local(const(env_ + ((n, val4))))(eval4(body))
                    }
                    case _ => throwError("type error in application")
                }
            }
        }
    }

    def testEval4 {
        val res = runEval4(Map.empty)(0)(eval4(exampleExp))
        expect((Right(IntVal(18)), 8))(res)
    }

    // 2.5 Adding Logging

    lazy val _M5 = MonadReader[Env, ReaderT.apply2[Env, ErrorT.apply2[String, WriterT.apply2[List[String], StateT.apply2[Int, Identity.type]]]]]
    type Eval5[+a] = _M5.apply[a]
    lazy val _M5E = MonadError[String, _M5.type]
    lazy val _M5W = MonadWriter[List[String], _M5.type]
    def runEval5[a](env: Env)(st: Int)(ev: Eval5[a]): ((Either[String, a], List[String]), Int) = (((ev.run)(env).run).run)(st).run

    lazy val eval5: Exp => Eval5[Value] = exp => {
        import _M5._
        import _M5E.throwError
        import _M5W.tell
        exp match {
            case Lit(i) => for {
                _ <- tick[Int, Eval5]
            } `return` { IntVal(i) }
            case Var(n) => for {
                _ <- tick[Int, Eval5]
                _ <- tell(List(n))
                env <- ask
            } `return` { env(n) }
            case Plus(e1, e2) => for {
                _ <- tick[Int, Eval5]
                e1_ <- eval5(e1)
                e2_ <- eval5(e2)
            } {
                (e1_, e2_) match {
                    case (IntVal(i1), IntVal(i2)) => `return` { IntVal(i1 + i2) }
                    case _ => throwError("type error in addition")
                }
            }
            case Abs(n, e) => for {
                _ <- tick[Int, Eval5]
                env <- ask
            } `return` { FunVal(env, n, e) }
            case App(e1, e2) => for {
                _ <- tick[Int, Eval5]
                val1 <- eval5(e1)
                val5 <- eval5(e2)
            } {
                val1 match {
                    case FunVal(env_, n, body) => {
                        local(const(env_ + ((n, val5))))(eval5(body))
                    }
                    case _ => throwError("type error in application")
                }
            }
        }
    }

    def testEval5 {
        val exp1 = Plus(Lit(12), App(Abs("x", Var("x")), Plus(Lit(4), Lit(2))))
        val exp2 = Plus(Lit(12), App(Abs("y", Var("y")), Plus(Lit(4), Lit(2))))
        val exp = Plus(exp1, exp2)
        val ((Right(IntVal(_)), List(String("x"), String("y"))), _) = runEval5(Map.empty)(0)(eval5(exp))
        ()
    }

    // 2.6 What about I/O?

    lazy val _M6 = MonadReader[Env, ReaderT.apply2[Env, ErrorT.apply2[String, WriterT.apply2[List[String], StateT.apply2[Int, IO.type]]]]]
    type Eval6[+a] = _M6.apply[a]
    lazy val _M6E = MonadError[String, _M6.type]
    lazy val _M6W = MonadWriter[List[String], _M6.type]
    lazy val _M6IO = MonadIO[_M6.type]
    def runEval6[a](env: Env)(st: Int)(ev: Eval6[a]): IO[((Either[String, a], List[String]), Int)] = (((ev.run)(env).run).run)(st)

    lazy val eval6: Exp => Eval6[Value] = exp => {
        import _M6._
        import _M6E.throwError
        import _M6W.tell
        import _M6IO.liftIO
        exp match {
            case Lit(i) => for {
                _ <- liftIO { IO.print(i) }
                _ <- tick[Int, Eval6]
            } `return` { IntVal(i) }
            case Var(n) => for {
                _ <- tick[Int, Eval6]
                _ <- tell(List(n))
                env <- ask
            } `return` { env(n) }
            case Plus(e1, e2) => for {
                _ <- tick[Int, Eval6]
                e1_ <- eval6(e1)
                e2_ <- eval6(e2)
            } {
                (e1_, e2_) match {
                    case (IntVal(i1), IntVal(i2)) => `return` { IntVal(i1 + i2) }
                    case _ => throwError("type error in addition")
                }
            }
            case Abs(n, e) => for {
                _ <- tick[Int, Eval6]
                env <- ask
            } `return` { FunVal(env, n, e) }
            case App(e1, e2) => for {
                _ <- tick[Int, Eval6]
                val1 <- eval6(e1)
                val6 <- eval6(e2)
            } {
                val1 match {
                    case FunVal(env_, n, body) => {
                        local(const(env_ + ((n, val6))))(eval6(body))
                    }
                    case _ => throwError("type error in application")
                }
            }
        }
    }

    def testEval6 {
        val res = runEval6(Map.empty)(0)(eval6(exampleExp))
        ignore {
            res.!
        }
    }
}
