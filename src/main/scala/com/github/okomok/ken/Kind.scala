

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


/**
 * Type-level utilities
 */
object Kind {
    @Annotation.marker
    trait FunctionLike

    // Functions
    //
    trait Function0 extends FunctionLike {
        type apply0
    }

    trait Function1 extends FunctionLike {
        type apply1[+a]
        type apply[+a] = apply1[a]
    }

    trait Function2 extends FunctionLike {
        type apply2[-a, +b]
    }

    type Function = Function1

    // Unapply
    //
    trait Unapply0 extends Function0

    trait Unapply1 extends Function1 {
        type arg1
        type arg = arg1
    }

    trait Unapply2 extends Function2 {
        type arg1
        type arg2
    }

    type Unapply = Unapply1

    // Newtypes
    //
    trait Newtype extends Function0 {
        type oldtype
        type deriving <: MethodList
    }

    trait Newtype1 extends Function1 {
        type oldtype1[+a]
    }

    trait Newtype2 extends Function2 {
        type oldtype2[-a, +b]
    }

    // coNewtypes
    //
    trait coNewtype[nt <: Newtype] extends Newtype {
        override type apply0 = nt#oldtype
        override type oldtype = nt#apply0
        override type deriving = nt#deriving
    }

    trait coNewtype1[nt <: Newtype1] extends Newtype1 {
        override type apply1[+a] = nt#oldtype1[a]
        override type oldtype1[+a] = nt#apply1[a]
    }

    trait coNewtype2[nt <: Newtype2] extends Newtype2 {
        override type apply2[-a, +b] = nt#oldtype2[a, b]
        override type oldtype2[-a, +b] = nt#apply2[a, b]
    }

    // MonadTrans
    //
    trait MonadTrans {
        type monadTrans[m[+_], +a]
    }

    // MonadFree
    //
    trait MonadFree {
        type freeFunctor[+a]
    }

    // Misc
    //
    trait const[z] extends Function0 with Function1 with Function2 {
        override type apply0 = z
        override type apply1[+a] = z
        override type apply2[-a, +b] = z
    }

    trait constThis extends Function0 with Function1 with Function2 {
        override type apply0 = this.type
        override type apply1[+a] = this.type
        override type apply2[-a, +b] = this.type
    }

    trait quote1[f[+_]] extends Function1 {
        override type apply1[+a] = f[a]
    }

    trait quote2[f[-_, +_]] extends Function2 {
        override type apply2[-a, +b] = f[a, b]
    }

    trait qcurry2[f[_, +_]] extends FunctionLike {
        trait apply[a] extends apply1[a]
        trait apply1[a] extends Function1 {
            override type apply1[+b] = f[a, b]
        }
    }

    type *[+a] = Any
    type **[-a, +b] = Any

    type Nothing1[+a] = Nothing
    type Nothing2[-a, +b] = Nothing

    // List
    //
    sealed trait List
    sealed trait ^:[x, xs <: List] extends List
    sealed trait Nil extends List with MethodList

    object List {
        trait Contains[xs <: List, y]

        object Contains {
            implicit def _ofHead[x, xs <: List]: Contains[x ^: xs, x] = new Contains[x ^: xs, x] {}
            implicit def _ofTail[x, xs <: List, y](implicit ev: Contains[xs, y]): Contains[x ^: xs, y] = new Contains[x ^: xs, y] {}
        }
    }

    // MethodList
    //
    sealed trait MethodList
    sealed trait ^::[f[_], fs <: MethodList] extends MethodList

    object MethodList {
        trait Contains[fs <: MethodList, g[_]]

        object Contains {
            implicit def _ofHead[f[_], fs <: MethodList]: Contains[f ^:: fs, f] = new Contains[f ^:: fs, f] {}
            implicit def _ofTail[f[_], fs <: MethodList, g[_]](implicit ev: Contains[fs, g]): Contains[f ^:: fs, g] = new Contains[f ^:: fs, g] {}
        }
    }
}
