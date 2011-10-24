

// Copyright Shunsuke Sogame 2011.
//
// Copyright 2008-2011 Edward Kmett
// Copyright 2004-2008 Dave Menendez
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait Extend[w[+_]] extends Functor[w] {
    final val asExtend: Extend[apply] = this

    // Core
    //
    def duplicate[a](w: w[a]): w[w[a]] = extend(id[w[a]])(w)
    def extend[a, b](f: w[a] => b): w[a] => w[b] = w => fmap(f)(duplicate(w))

    // Extra
    //
    @Annotation.aliasOf("extend")
    final def op_<<=:[a, b](f: w[a] => b): w[a] => w[b] = extend(f)

    @Annotation.flipOf("extend")
    final def op_=>>[a, b](w: w[a])(f: w[a] => b): w[b] = extend(f)(w)

    /** Cokleisli composition */
    def op_=<=:[a, b, c](f: w[b] => c)(g: w[a] => b): w[a] => c = f `.` extend(g)

    @Annotation.flipOf("op_=<=:")
    final def op_=>=:[a, b, c](g: w[a] => b)(f: w[b] => c): w[a] => c = op_=<=:(f)(g)

    // Operators
    //
    private[ken] sealed class Op_<<=:[a](w: w[a]) {
        def <<=:[b](f: w[a] => b): w[b] = op_<<=:(f)(w)
    }
    final implicit def <<=:[a](w: w[a]): Op_<<=:[a] = new Op_<<=:(w)

    private[ken] sealed class Op_=>>[a](w: w[a]) {
        def =>>[b](f: w[a] => b): w[b] = op_=>>(w)(f)
    }
    final implicit def =>>[a](w: w[a]): Op_=>>[a] = new Op_=>>(w)

    private[ken] sealed class Op_=<=:[a, b](g: w[a] => b) {
        def =<=:[c](f: w[b] => c): w[a] => c = op_=<=:(f)(g)
    }
    final implicit def =<=:[a, b](g: w[a] => b): Op_=<=:[a, b] = new Op_=<=:(g)

    private[ken] sealed class Op_=>=:[b, c](f: w[b] => c) {
        def =>=:[a](g: w[a] => b): w[a] => c = op_=>=:(g)(f)
    }
    final implicit def =>=:[b, c](f: w[b] => c): Op_=>=:[b, c] = new Op_=>=:(f)
}


trait ExtendProxy[w[+_]] extends Extend[w] with FunctorProxy[w] {
    def selfExtend: Extend[w]
    override def selfFunctor: Functor[w] = selfExtend

    override def duplicate[a](w: w[a]): w[w[a]] = selfExtend.duplicate(w)
    override def extend[a, b](f: w[a] => b): w[a] => w[b] = selfExtend.extend(f)

    override def op_=<=:[a, b, c](f: w[b] => c)(g: w[a] => b): w[a] => c = selfExtend.op_=<=:(f)(g)
}


object Extend extends ExtendInstance {
    def apply[w <: Kind.Function1](implicit i: Extend[w#apply1]): Extend[w#apply1] = i

    def deriving[nt <: Kind.Newtype1](implicit j: Newtype1[nt#apply1, nt#oldtype1], i: Extend[nt#oldtype1]): Extend[nt#apply1] = new Extend[nt#apply1] with FunctorProxy[nt#apply1] {
        private type w[+a] = nt#apply1[a]
        override val selfFunctor = Functor.deriving[nt]

        override def extend[a, b](f: w[a] => b): w[a] => w[b] = nt => j.newOf(i.extend((ot: nt#oldtype1[a]) => f(j.newOf(ot)))(j.oldOf(nt)))
    }

    def weak[nt <: Kind.Newtype1](implicit j: Newtype1[nt#apply1, nt#oldtype1], i: Extend[nt#apply1]): Extend[nt#oldtype1] = deriving[Kind.coNewtype1[nt]](j.coNewtype, i)
}


trait ExtendInstance { this: Extend.type =>
    implicit def ofFunction[z](implicit i: Semigroup[z]): Extend[Function.apply[z]#apply1] = Function._asExtend(i)
}
