

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class MiniMonadTransTest extends org.scalatest.junit.JUnit3Suite {
    def test_ {}

    trait MiniMonadTransControl[t[_[+_], _]] {
        def lift[m[+_], a](m: m[a]): t[m, a]
    }

    trait KindMiniMonadTransControl {
        type trans[m[+_], a]
    }

    object MiniMonadTransControl {
        def apply[t <: KindMiniMonadTransControl](implicit i: MiniMonadTransControl[t#trans]): MiniMonadTransControl[t#trans] = i
    }


    trait MyParser[s, +a]

    object MyParser extends Kind.FunctionLike {
        sealed trait apply[s] extends Kind.Function1 {
            override type apply1[+a] = MyParser[s, a]
        }

        implicit def _asMonad[s]: Monad[apply[s]#apply1] = error("todo")
    }

    final case class StateT[s, m[+_], +a](override val old: s => m[(a, s)]) extends NewtypeOf[s => m[(a, s)]]

    object StateT extends Kind.FunctionLike with KindMiniMonadTransControl {
        sealed trait apply2[s, m[+_]] extends Kind.Function1 {
            override type apply1[+a] = StateT[s, m, a]
        }
        sealed trait apply[s] extends KindMiniMonadTransControl {
            override type trans[m[+_], a] = StateT[s, m, a]
        }
        implicit def _asMiniMonadTransControl[s]: MiniMonadTransControl[apply[s]#trans] = error("todo")
        //implicit def _asMiniMonadTransControl[s]: MiniMonadTransControl[({type t[m[+_], a] = StateT[s, m, a]})#t] = error("todo")
        implicit def _asMonad[s, m[+_]]: Monad[apply2[s, m]#apply1] = error("todo")
    }

    def teztTrivial {
        MiniMonadTransControl[StateT.apply[Int]]
        instance[MiniMonadTransControl[({type t[m[+_], a] = StateT[Int, m, a]})#t]]

        Monad[StateT.apply2[Int, IO]]
        instance[Monad[({type m[+a] = StateT[Int, IO, a]})#m]]

        Monad[StateT.apply2[Int, WeakIdentity.apply]]
        // instance[Monad[({type m[+a] = StateT[Int, WeakIdentity.apply, a]})#m]] // no

        val i = Monad[MyParser.apply[String]]
        val j = instance[Monad[({type m[+a] = MyParser[String, a]})#m]]
        Monad[StateT.apply2[Int, i.apply]]
        Monad[StateT.apply2[Int, j.apply]]
    }

}
