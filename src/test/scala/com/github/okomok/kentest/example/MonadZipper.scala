// Public domain


package com.github.okomok.kentest.example


import com.github.okomok.ken._


// http://tomschrijvers.blogspot.com/2010/03/bruno-oliveira-and-i-are-working-on.html


class MonadZipperTest extends org.scalatest.junit.JUnit3Suite {

    // 1. Introduction

    final case class Var[+e](_1: String)
    final case class Add[+e](_1: e, _2: e)

    sealed abstract class Sum[+f[+_], +g[+_], +e]
    final case class Inl[f[+_], e](_1: f[e]) extends Sum[f, Kind.Nothing1, e]
    final case class Inr[g[+_], e](_1: g[e]) extends Sum[Kind.Nothing1, g, e]

    final case class Fix[f[+_]](out: f[Fix[f]])

    type AddSumVar[+e] = Sum[Add, Var, e]
    type Expr0 = Fix[AddSumVar]

    type Env = List[(String, Int)]

    val em = MonadError[String, Error.apply[String]]
    implicit val sm = MonadState[Env, StateT.apply2[Env, em.type]]
    type M[+a] = sm.apply[a]

    object Introduction {

        lazy val evalVar: Var[Expr0] => M[Int] = {
            case Var(v) => {
                import sm.`for`
                for {
                    env <- sm.get
                } {
                    List.lookup(v)(env) match {
                        case Nothing => StateT { _ => em.throwError("Variable does not exist!") }
                        case Just(x) => sm.`return`(x)
                    }
                }
            }
        }

        lazy val evalAdd: Add[Expr0] => M[Int] = {
            case Add(l, r) => {
                import sm.`for`
                for {
                    x <- eval0(l)
                    y <- eval0(r)
                } yield (x + y)
            }
        }

        lazy val eval0: Expr0 => M[Int] = e => {
            e.out match {
                case Inl(e) => evalAdd(e)
                case Inr(e) => evalVar(e)
            }
        }

        lazy val add0: Expr0 => Expr0 => Expr0 = e1 => e2 => Fix[AddSumVar](Inl(Add(e1, e2)))
        lazy val var0: String => Expr0 = v => Fix[AddSumVar](Inr(Var(v)))
    }

    final case class Lit[+e](_1: Int)

    // 2. Dependency Abstraction

    type Open[e, f[+_], r] = (e => r) => f[e] => r

    def evalAdd[e](eval: e => M[Int]): Add[e] => M[Int] = {
        case Add(l, r) => {
            import sm.`for`
            for {
                x <- eval(l)
                y <- eval(r)
            } yield (x + y)
        }
    }

    def evalVar[e](eval: e => M[Int]): Var[e] => M[Int] = error("todo")
    def evalLit[e](eval: e => M[Int]): Lit[e] => M[Int] = error("todo")

    def compose[e, f[+_], g[+_], r](evalf: Open[e, f, r])(evalg: Open[e, g, r]): Open[e, ({type h[+e] = Sum[f, g, e]})#h, r] = eval => e => {
        e match {
            case Inl(el) => evalf(eval)(el)
            case Inr(er) => evalg(eval)(er)
        }
    }

    // Open[Fix[f], f, r]
    //   =:=
    // (Fix[f] => r) => f[Fix[f]] => r
    def fix[f[+_], r](f: Open[Fix[f], f, r]): Fix[f] => r = x => f(fix(f))(x.out)

    type AddSumVarSumLit[+x] = Sum[AddSumVar, Lit, x]

    type Expr1 = Fix[AddSumVarSumLit]

    lazy val eval1: Expr1 => M[Int] = fix { (me: Fix[AddSumVarSumLit] => M[Int]) =>
        val tmp1: Open[Expr1, AddSumVar, M[Int]] = compose[Expr1, Add, Var, M[Int]](evalAdd)(evalVar)
        val tmp2: Open[Expr1, AddSumVarSumLit, M[Int]] = compose[Expr1, AddSumVar, Lit, M[Int]](tmp1)(evalLit)
        tmp2(me)
    }

    // 2.5 Mission Accomplished?

    sealed abstract class Mem[+e]
    type Reg = Int

    def evalMem[e, m[+_]](eval: e => m[Int])(implicit _sm: MonadState[Reg, m]): Mem[e] => m[Int] = error("todo")

    type MemSumVar[+x] = Sum[Mem, Var, x]
    type MemSumVarSumLit[+x] = Sum[MemSumVar, Lit, x]

    type Exprs = Fix[MemSumVarSumLit]
    val sms = MonadState[Reg, StateT.apply2[Reg, sm.type]]
    type Ms[+a] = sms.apply[a]

    // evalAdd[e] : (e => M[Int]) => Add[e] => M[Int]
    // evalAdds[e] : (e => Ms[Int]) => Add[e] => Ms[Int]
    //
    // In order to make `evalAdds` from `evalAdd`, you need "unlift" in addition to `MonadTrans.lift`.
    // M[Int] --MonadTrans.lift--> Ms[Int]
    // Ms[Int] --UNLIFT--> M[Int]
    // Probably `MonadTrans` can kick in.

    // 3. The Monad Zipper

    final case class ZipperT[t1[_[+_], +_], t2[_[+_], +_], n[+_], +a](run: t1[({type n_[+x] = t2[n, x]})#n_, a])

    object ZipperT {
        implicit def _asMonadTrans[t1[_[+_], +_], t2[_[+_], +_]](implicit _mt1: MonadTrans[t1], _mt2: MonadTrans[t2]): MonadTrans[({type t[n[+_], +a] = ZipperT[t1, t2, n, a]})#t] = new MonadTrans[({type t[n[+_], +a] = ZipperT[t1, t2, n, a]})#t] {
            // MonadTrans
            private type t[n[+_], +a] = ZipperT[t1, t2, n, a]
            override def lift[n[+_], a](n: n[a])(implicit _mn: Monad[n]): t[n, a] = {
                val m2: t2[n, a] = _mt2.lift(n)(_mn)
                val m1: t1[({type n_[+x] = t2[n, x]})#n_, a] = _mt1.lift[({type n_[+x] = t2[n, x]})#n_, a](m2)(undefined/*hmm*/)
                ZipperT[t1, t2, n, a](m1)
            }
            override def liftWith[n[+_], a](f: Run => n[a])(implicit i: Monad[n]): t[n, a] = error("todo")
        }
    }

    // given up.

    def testTrivial {
    }



}
