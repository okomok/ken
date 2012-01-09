

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


// See: http://apocalisp.wordpress.com/2011/10/26/tail-call-elimination-in-scala-monads/


sealed abstract class Trampoline[+a] extends Eval[a] {
    override lazy val _eval: a = Trampoline.__eval(this)
}


object Trampoline extends Monad[Trampoline] with ThisIsInstance with EvalOp {
    // Overrides
    //
    // Monad
    private type m[+a] = Trampoline[a]
    override def `return`[a](a: Lazy[a]): m[a] = new Done(a)
    override def op_>>=[a, b](t: m[a])(k: a => m[b]): m[b] = new Cont(t, k)

    // For the IORep implementation, you can't use case-classes, which extend ProductN.
    private[ken] class Done[+a](val result: a) extends Trampoline[a]
    private[ken] class Cont[z, +a](val current: Trampoline[z], val next: z => Trampoline[a]) extends Trampoline[a]

    private def __eval[a](t: Trampoline[a]): a = {
        var cur: Trampoline[_] = t
        val stack = new java.util.ArrayDeque[Any => Trampoline[a]]
        var result: Maybe[a] = Nothing

        while (Maybe.isNothing(result)) {
            cur match {
                case c: Done[_] => {
                    if (stack.isEmpty) {
                        result = Just(c.result.asInstanceOf[a])
                    } else {
                        cur = (stack.pop())(c.result)
                    }
                }
                case c: Cont[_, _] => {
                    cur = c.current
                    stack.push(c.next.asInstanceOf[Any => Trampoline[a]])
                }
            }
        }

        Maybe.fromJust(result)
    }
}
