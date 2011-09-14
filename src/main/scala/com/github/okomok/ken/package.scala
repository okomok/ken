

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok


package object ken {

    // Eq
    //
    type Eq[a] = _Eq[a]
    val Eq = _Eq

    // Bools
    //
    final val otherwise = True

    // Miscellaneous functions
    //
    def id[a]: a => a = x => x

    def const[a](x: a): Any => a = _ => x

    def `op_.`[a, b, c](f: b => c)(g: a => b): a => c = x => f(g(x))

    sealed class `Op_.`[b, c](f: b => c) {
        def `.`[a](g: a => b): a => c = `op_.`(f)(g)
    }
    implicit def `.`[b, c](f: b => c): `Op_.`[b, c] = new `Op_.`(f)

    def flip[a, b, c](f: a => b => c): b => a => c = x => y => f(y)(x)

    def op_@[a, b](f: a => b)(x: a): b = f(x)

    def until[a](p: a => Bool)(f: a => a)(x: a): a = {
        if (p(x)) x else until(p)(f)(f(x))
    }

    def asTypeOf[a](x: a)(y: => a): a = x

    sealed class AsTypeOf[a](x: a) {
        def _asTypeOf_(y: => a): a = x
    }
    implicit def _asTypeOf_[a](x: a): AsTypeOf[a] = new AsTypeOf(x)

    val error: String => Nothing = { msg => throw new java.lang.Error(List.toJString(msg)) }

    def undefined: Nothing = throw new java.lang.Error("undefined")

    def seq[b](x: Any)(y: b): b = y // no special effects

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
    type String = List[Char]

    type Bool = Boolean
    val Bool = _Bool

    final val True = true
    final val False = false

    type Integer = BigInt
    val Integer = _Integer

    val Pair = Tuple2

    type Rational = Ratio[Integer]

    type ReadS[+a] = String => List[(a, String)]

    type ShowS = String => String

    type IOError = java.io.IOException // TODO

    type IORep[+a] = RealWorld.type => (a, RealWorld.type)

    type TypeRep = ClassManifest[_]

    // Scala specific utilities
    //
    val ignore: (=> Any) => Unit = _ => ()

    def instance[a](implicit i: a): a = i
}
