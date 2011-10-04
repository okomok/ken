

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok


package object ken {

    // Eq
    //
    type Eq[-a] = _Eq[a]
    val Eq = _Eq

    def op_===[a](x: a)(y: a)(implicit i: _Eq[a]): Bool = i.op_===(x)(y)
    def op_/==[a](x: a)(y: a)(implicit i: _Eq[a]): Bool = i.op_/==(x)(y)

    private[ken] sealed class _Op_===[a](x: a)(implicit i: _Eq[a]) {
        def ===(y: a): Bool = op_===(x)(y)
    }
    implicit def ===[a](x: a)(implicit i: _Eq[a]): _Op_===[a] = new _Op_===(x)

    private[ken] sealed class _Op_/==[a](x: a)(implicit i: _Eq[a]) {
        def /==(y: a): Bool = op_/==(x)(y)
    }
    implicit def /==[a](x: a)(implicit i: _Eq[a]): _Op_/==[a] = new _Op_/==(x)

    // Show
    //
    type ShowS = String => String

    def show[a](s: a)(implicit i: Show[a]): String = i.show(s)

    // Miscellaneous functions
    //
    def id[a]: a => a = x => x

    def const[a](x: a): Any => a = _ => x

    def `op_.`[a, b, c](f: b => c)(g: a => b): a => c = x => f(g(x))

    private[ken] sealed class `Op_.`[b, c](f: b => c) {
        def `.`[a](g: a => b): a => c = `op_.`(f)(g)
    }

    @Annotation.ceremonial("`compose` is recommended")
    implicit def `.`[b, c](f: b => c): `Op_.`[b, c] = new `Op_.`(f)

    def flip[a, b, c](f: a => b => c): b => a => c = x => y => f(y)(x)

    def op_@[a, b](f: a => b)(x: a): b = f(x)

    private[ken] sealed class Op_@[a, b](f: a => b) {
        def `@`(x: a): b = op_@(f)(x)
    }

    @Annotation.ceremonial("`apply` is much better")
    implicit def `@`[a, b](f: a => b): Op_@[a, b] = new Op_@(f)

    def until[a](p: a => Bool)(f: a => a)(x: a): a = {
        if (p(x)) x else until(p)(f)(f(x))
    }

    @Annotation.ceremonial("useless in Scala")
    def asTypeOf[a](x: a)(y: Lazy[a]): a = x

    private[ken] sealed class Op_asTypeOf_[a](x: a) {
        def _asTypeOf_(y: Lazy[a]): a = x
    }

    @Annotation.ceremonial("useless in Scala")
    implicit def _asTypeOf_[a](x: a): Op_asTypeOf_[a] = new Op_asTypeOf_(x)

    val error: String => Nothing = { msg => throw new java.lang.Error(List.toJString(msg)) }

    def undefined: Nothing = throw new java.lang.Error("undefined")

    @Annotation.ceremonial("no special effects")
    def seq[b](x: Any)(y: b): b = y

    @Annotation.ceremonial("same as `op_@`")
    def op_@![a, b](f: a => b)(x: a): b = seq(x)(f(x))

    @Annotation.ceremonial("same as `@`")
    implicit def @![a, b](f: a => b): Op_@[a, b] = new Op_@(f)

    // Trivial transformers
    //
    type Error[e, +a] = ErrorT[e, WeakIdentity.apply, a]
    val Error = _Error

    type State[s, +a] = StateT[s, WeakIdentity.apply, a]
    val State = _State

    type Reader[r, +a] = WeakIdentity.ReaderT[r, a]
    val Reader = WeakIdentity.ReaderT

    type Writer[w, +a] = WeakIdentity.WriterT[w, a]
    val Writer = WeakIdentity.WriterT

    // Aliases
    //
    type String = List[Char]
    type JString = Predef.String

    type Bool = Boolean
    val Bool = _Bool

    final val True = true
    final val False = false

    type Integer = BigInt
    val Integer = _Integer

    val Pair = Tuple2
    // val Triple = Tuple3

    type Rational = Ratio[Integer]

    type ReadS[+a] = String => List[(a, String)]

    type IOError = IOException
    val IOError = IOException

    @Annotation.ceremonial("no special effects")
    type IORep[+a] = RealWorld.type => (a, RealWorld.type)

    type TypeRep = ClassManifest[_]

    // Unsafe.Coerce
    //
    def unsafeCoerce[a, b](a: a, * : Type[b] = null): b = a.asInstanceOf[b]

    // Scala specific utilities
    //
    val ignore: (=> Any) => Unit = _ => ()

    def instance[a](implicit i: a): a = i

    // Kind
    //
    type ^:[x, xs <: Kind.List] = Kind.^:[x, xs]
    type ^::[f[_], fs <: Kind.MethodList] = Kind.^::[f, fs]
}
