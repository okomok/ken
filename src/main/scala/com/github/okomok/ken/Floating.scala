

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait Floating[a] extends Fractional[a] {
    final val asFloating: Floating[apply0] = this

    // Core
    //
    def pi: a

    def exp: a => a
    def log: a => a
    def sqrt: a => a = x => x ** 0.5

    def op_** : a => a => a = x => y => exp(log(x) * y)
    def logBase: a => a => a = x => y => log(y) / log(x)

    def sin: a => a
    def cos: a => a
    def tan: a => a = x => sin(x) / cos(x)

    def asin: a => a
    def acos: a => a
    def atan: a => a

    def sinh: a => a
    def cosh: a => a
    def tanh: a => a = x => sinh(x) / cosh(x)

    def asinh: a => a
    def acosh: a => a
    def atanh: a => a

    // Operators
    //
    sealed class Op_**(x: a) {
        def **(y: a): a = op_**(x)(y)
    }
    final implicit def **(x: a): Op_** = new Op_**(x)
}


trait FloatingProxy[a] extends Floating[a] with FractionalProxy[a] {
    def selfFloating: Floating[a]
    override def selfFractional: Fractional[a] = selfFloating

    override def pi: a = selfFloating.pi

    override def exp: a => a = selfFloating.exp
    override def log: a => a = selfFloating.log
    override def sqrt: a => a = selfFloating.sqrt

    override def op_** : a => a => a = selfFloating.op_**
    override def logBase: a => a => a = selfFloating.logBase

    override def sin: a => a = selfFloating.sin
    override def cos: a => a = selfFloating.cos
    override def tan: a => a = selfFloating.tan

    override def asin: a => a = selfFloating.asin
    override def acos: a => a = selfFloating.acos
    override def atan: a => a = selfFloating.atan

    override def sinh: a => a = selfFloating.sinh
    override def cosh: a => a = selfFloating.cosh
    override def tanh: a => a = selfFloating.tanh

    override def asinh: a => a = selfFloating.asinh
    override def acosh: a => a = selfFloating.acosh
    override def atanh: a => a = selfFloating.atanh
}


object Floating {
    def apply[a <: Kind.Function0](implicit i: Floating[a#apply0]): Floating[a#apply0] = i
}
