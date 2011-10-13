

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class MiniMonadTransTest extends org.scalatest.junit.JUnit3Suite {
    def test_ {}

    trait MiniMonadTrans[t[_[+_], _]] {
        def lift[m[+_], a](m: m[a]): t[m, a]
    }

    trait KindMiniMonadTrans {
        type trans[m[+_], a]
    }

    object MiniMonadTrans {
        def apply[t <: KindMiniMonadTrans](implicit i: MiniMonadTrans[t#trans]): MiniMonadTrans[t#trans] = i
    }


    trait MyParser[s, +a]

    object MyParser extends Kind.FunctionLike {
        sealed trait apply[s] extends Kind.Function1 {
            override type apply1[+a] = MyParser[s, a]
        }

        implicit def _asMonad[s]: Monad[apply[s]#apply] = error("todo")
    }

    final case class StateT[s, m[+_], +a](override val old: s => m[(a, s)]) extends NewtypeOf[s => m[(a, s)]]

    object StateT extends Kind.FunctionLike with KindMiniMonadTrans {
        sealed trait apply2[s, m[+_]] extends Kind.Function1 {
            override type apply1[+a] = StateT[s, m, a]
        }
        sealed trait apply[s] extends KindMiniMonadTrans {
            override type trans[m[+_], a] = StateT[s, m, a]
        }
        implicit def _asMiniMonadTrans[s]: MiniMonadTrans[apply[s]#trans] = error("todo")
        //implicit def _asMiniMonadTrans[s]: MiniMonadTrans[({type t[m[+_], a] = StateT[s, m, a]})#t] = error("todo")
        implicit def _asMonad[s, m[+_]]: Monad[apply2[s, m]#apply1] = error("todo")
    }

    def teztTrivial {
        MiniMonadTrans[StateT.apply[Int]]
        instance[MiniMonadTrans[({type t[m[+_], a] = StateT[Int, m, a]})#t]]

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
