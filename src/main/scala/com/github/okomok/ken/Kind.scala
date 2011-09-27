

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
        type apply[+a] = apply1[a] // alias of apply1
    }

    trait Function2 extends FunctionLike {
        type apply2[-a, +b]
    }

    // Newtypes
    //
    trait Newtype0 extends Function0 {
        type oldtype0
        type deriving0 <: MethodList
    }

    trait Newtype1 extends Function1 {
        type oldtype1[+a]
    }

    trait Newtype2 extends Function2 {
        type oldtype2[-a, +b]
    }

    // coNewtypes
    //
    trait coNewtype0[nt <: Newtype0] extends Newtype0 {
        override type apply0 = nt#oldtype0
        override type oldtype0 = nt#apply0
        override type deriving0 = nt#deriving0
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
    trait MonadTrans extends Newtype1 {
        type innerMonad[+a]
    }

    // MonadTransControl
    //
    trait MonadTransControl extends MonadTrans {
        type baseResult[+a]
    }

    // MonadT
    //
    trait MonadT extends MonadTrans {
        //type oldtype1[+a] = innerMonad[baseMonad[a]]
        type baseMonad[+a]
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
        trait apply[a] extends Function1 {
            override type apply1[+b] = f[a, b]
        }
    }

    // List
    //
    sealed trait List
    sealed trait Cons[x, xs <: List] extends List
    sealed trait Nil extends List with MethodList

    type List1[x1] = Cons[x1, Nil]
    type List2[x1, x2] = Cons[x1, Cons[x2, Nil]]
    type List3[x1, x2, x3] = Cons[x1, Cons[x2, Cons[x3, Nil]]]
    type List4[x1, x2, x3, x4] = Cons[x1, Cons[x2, Cons[x3, Cons[x4, Nil]]]]
    type List5[x1, x2, x3, x4, x5] = Cons[x1, Cons[x2, Cons[x3, Cons[x4, Cons[x5, Nil]]]]]

    object List {
        trait Contains[xs <: List, y]

        object Contains {
            implicit def ofHead[x, xs <: List]: Contains[Cons[x, xs], x] = new Contains[Cons[x, xs], x] {}
            implicit def ofTail[x, xs <: List, y](implicit ev: Contains[xs, y]): Contains[Cons[x, xs], y] = new Contains[Cons[x, xs], y] {}
        }
    }

    // MethodList (for cute syntax, but may be rejected.)
    //
    sealed trait MethodList
    sealed trait MethodCons[f[_], fs <: MethodList] extends MethodList

    type MethodList1[f1[_]] = MethodCons[f1, Nil]
    type MethodList2[f1[_], f2[_]] = MethodCons[f1, MethodCons[f2, Nil]]
    type MethodList3[f1[_], f2[_], f3[_]] = MethodCons[f1, MethodCons[f2, MethodCons[f3, Nil]]]
    type MethodList4[f1[_], f2[_], f3[_], f4[_]] = MethodCons[f1, MethodCons[f2, MethodCons[f3, MethodCons[f4, Nil]]]]
    type MethodList5[f1[_], f2[_], f3[_], f4[_], f5[_]] = MethodCons[f1, MethodCons[f2, MethodCons[f3, MethodCons[f4, MethodCons[f5, Nil]]]]]

    object MethodList {
        trait Contains[fs <: MethodList, y[_]]

        object Contains {
            implicit def ofHead[f[_], fs <: MethodList]: Contains[MethodCons[f, fs], f] = new Contains[MethodCons[f, fs], f] {}
            implicit def ofTail[f[_], fs <: MethodList, y[_]](implicit ev: Contains[fs, y]): Contains[MethodCons[f, fs], y] = new Contains[MethodCons[f, fs], y] {}
        }
    }
}
