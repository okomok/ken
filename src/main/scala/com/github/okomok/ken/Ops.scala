

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


private[ken] class _Op_===[a](x: a)(implicit i: _Eq[a]) {
    def ===(y: a): Bool = op_===(x)(y)
}

private[ken] class _Op_/==[a](x: a)(implicit i: _Eq[a]) {
    def /==(y: a): Bool = op_/==(x)(y)
}

private[ken] class `Op_.`[b, c](f: b => c) {
    def `.`[a](g: a => b): a => c = `op_.`(f)(g)
}

private[ken] class Op_@[a, b](f: a => b) {
    def `@`(x: a): b = op_@(f)(x)
}

private[ken] class Op_asTypeOf_[a](x: a) {
    def _asTypeOf_(y: Lazy[a]): a = x
}
