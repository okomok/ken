

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


// See: http://apocalisp.wordpress.com/2011/10/26/tail-call-elimination-in-scala-monads/


// For the IORep implementation, you can't use case-classes directly, which extend ProductN.
class Trampoline[+a](private val impl: Trampoline.Impl[a]) extends Eval[a] {
    override lazy val _eval: a = Trampoline.__eval(this)
}


object Trampoline extends Monad[Trampoline] with ThisIsInstance with EvalOp {
    // Overrides
    //
    // Monad
    private type m[+a] = Trampoline[a]
    override def `return`[a](a: Lazy[a]): m[a] = new Trampoline(Done(a))
    override def op_>>=[a, b](t: m[a])(k: a => m[b]): m[b] = new Trampoline(Cont(t, k))

    private[ken] sealed abstract class Impl[+a]
    private[ken] case class Done[+a](a: a) extends Impl[a]
    private[ken] case class Cont[z, +a](c: Trampoline[z], k: z => Trampoline[a]) extends Impl[a]

    private def __eval[a](t: Trampoline[a]): a = {
        var cur: Trampoline[_] = t
        val stack = new java.util.ArrayDeque[Any => Trampoline[a]]
        var result: Maybe[a] = Nothing

        while (Maybe.isNothing(result)) {
            cur.impl match {
                case Done(a) => {
                    if (stack.isEmpty) {
                        result = Just(a.asInstanceOf[a])
                    } else {
                        cur = (stack.pop())(a)
                    }
                }
                case Cont(c, k) => {
                    cur = c
                    stack.push(k.asInstanceOf[Any => Trampoline[a]])
                }
            }
        }

        Maybe.fromJust(result)
    }
}
