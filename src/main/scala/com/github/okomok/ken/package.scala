

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok


package object ken {

    // Eq
    //
    type Eq[-a] = _Eq[a]
    val Eq = _Eq

    // Bools
    //
    val not: Bool => Bool = { b => !b }

    val op_&& : Bool => Lazy[Bool] => Bool = { b => c => b && c.! }

    val op_|| : Bool => Lazy[Bool] => Bool = { b => c => b || c.! }

    final val otherwise = True

    // Tuples
    //
    def fst[a](p: (a, _)): a = p match {
        case (x, _) => x
    }

    def snd[b](p: (_, b)): b = p match {
        case (_, y) => y
    }

    def curry[a, b, c](f: Tuple2[a, b] => c): a => b => c = { x => y => f((x, y)) }

    def uncurry[a, b, c](f: a => b => c): Tuple2[a, b] => c = { case (x, y) => f(x)(y) }

    def swap[a, b](p: (a, b)): (b, a) = p match {
        case (x, y) => (y, x)
    }

    // Miscellaneous functions
    //
    def id[a]: a => a = x => x

    def const[a](x: a): Any => a = { _ => x }

    def op_compose[a, b, c](f: b => c)(g: a => b): a => c = { x => f(g(x)) }

    def flip[a, b, c](f: a => b => c): b => a => c = { x => y => f(y)(x) }

    def op_@[a, b](f: a => b)(x: a): b = f(x)

    def until[a](p: a => Bool)(f: a => a)(x: a): a = {
        if (p(x)) x else until(p)(f)(f(x))
    }

    def asTypeOf[a](x: a): a => a = const(x)

    val error: String_ => Nothing = { msg => throw new java.lang.Error(List.stringize(msg)) }

    def undefined: Nothing = throw new java.lang.Error("undefined")

    def seq[b](x: Any)(y: b): b = y // no special effects

    // Converting to String
    //
    def show[a](x: a)(implicit i: Show[a]): String_ = i.show(x)

    val _show: Any => String = _.toString // will be removed.

    // Trivial transformers
    //
    type Error[e, +a] = WeakIdentity.ErrorT[e, a]
    val Error = WeakIdentity.ErrorT

    type State[s, +a] = WeakIdentity.StateT[s, a]
    val State = WeakIdentity.StateT

    type Reader[r, +a] = WeakIdentity.ReaderT[r, a]
    val Reader = WeakIdentity.ReaderT

    type Writer[w, +a] = WeakIdentity.WriterT[w, a]
    val Writer = WeakIdentity.WriterT

    // Aliases
    //
    type String_ = List[Char]
    type Bool = Boolean
    final val True = true
    final val False = false
    type Integer = BigInt

    // Scala specific utilities
    //
    val ignore: (=> Any) => Unit = { _ => () }

    def instance[a](implicit i: a): a = i
}
