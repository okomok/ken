

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait Eval[+a] {
    private[ken] def _eval: a
}


trait EvalOp extends EvalLowImplicit {
    implicit def eval[a](x: Eval[a]): a = x._eval // higher priority
}

private[ken] sealed trait EvalLowImplicit {
    private[ken] sealed class Op_![a](x: Eval[a]) {
        def ! : a = x._eval
    }
    final implicit def _bang[a](x: Eval[a]): Op_![a] = new Op_!(x) // lower priority
}
