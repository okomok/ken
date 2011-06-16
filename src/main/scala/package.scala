

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok


import scala.annotation.tailrec


package object ken {

// Eq
    type Eq[a] = _Eq.Eq[a]
    val Eq = _Eq.Eq

// Booleans
    def not(b: Boolean): Boolean = !b

    def op_&&(b: Boolean)(c: => Boolean): Boolean = b && c

    def op_||(b: Boolean)(c: => Boolean): Boolean = b || c

// Tuples
    def fst[a, b](p: (a, b)): a = p match {
        case (x, _) => x
    }

    def snd[a, b](p: (a, b)): b = p match {
        case (_, y) => y
    }

    def curry[a, b, c](f: (a, b) => c): a => b => c = { x => y => f(x, y) }

    def uncurry[a, b, c](f: a => b => c): (a, b) => c = { (x, y) => f(x)(y) }

// Miscellaneous functions
    def id[a](x: a): a = x

    def const[a](x: a): Any => a = { _ => x }

    def op_compose[a, b, c](f: b => c)(g: a => b): a => c = { x => f(g(x)) }

    def flip[a, b, c](f: a => b => c): b => a => c = { x => y => f(y)(x) }

    def `@`[a, b](f: a => b)(x: a): b = f(x)

    def until[a](p: a => Boolean)(f: a => a)(x: a): a = {
        if (p(x)) x else until(p)(f)(f(x))
    }

    def asTypeOf[a](x: a): a => a = const(x)

    def error(msg: String): Nothing = throw new Error(msg)

    def undefined: Nothing = throw new Error("undefined")

// Converting to String
    def show(x: Any): String = x.toString
}
