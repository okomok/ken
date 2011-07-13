

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok


package object ken {

// String
    type StringT = List[Char]

// Eq
    type Eq[a] = _Eq.Eq[a]
    val Eq = _Eq.Eq

// Booleans
    val not: Boolean => Boolean = { b => !b }

    val op_&& : Boolean => (=> Boolean) => Boolean = { b => c => b && c }

    val op_|| : Boolean => (=> Boolean) => Boolean = { b => c => b || c }

    final val otherwise = true

// Tuples
    def fst[a, b](p: (a, b)): a = p match {
        case (x, _) => x
    }

    def snd[a, b](p: (a, b)): b = p match {
        case (_, y) => y
    }

    def curry[a, b, c](f: (a, b) => c): a => b => c = { x => y => f(x, y) }

    def uncurry[a, b, c](f: a => b => c): (a, b) => c = { (x, y) => f(x)(y) }

    def curry2[a, b, c](f: Tuple2[a, b] => c): a => b => c = { x => y => f((x, y)) }

    def uncurry2[a, b, c](f: a => b => c): Tuple2[a, b] => c = { case (x, y) => f(x)(y) }

    def swap[a, b](p: (a, b)): (b, a) = p match {
        case (x, y) => (y, x)
    }

// Miscellaneous functions
    def id[a](x: a): a = x

    def const[a](x: a): Any => a = { _ => x }

    def op_compose[a, b, c](f: b => c)(g: a => b): a => c = { x => f(g(x)) }

    def flip[a, b, c](f: a => b => c): b => a => c = { x => y => f(y)(x) }

    def op_@[a, b](f: a => b)(x: a): b = f(x)

    def op_@![a, b](f: a => b)(x: a): b = f(x) // same as op_@

    def until[a](p: a => Boolean)(f: a => a)(x: a): a = {
        if (p(x)) x else until(p)(f)(f(x))
    }

    def asTypeOf[a](x: a): a => a = const(x)

    val error: String => Nothing = { msg => throw new Error(msg) }

    def undefined: Nothing = throw new Error("undefined")

    def seq[a, b](x: a)(y: b): b = y // no effects

// Converting to String
    val show: Any => String = _.toString
}
