

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
        type deriving0 <: List
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

    // Misc
    //
    trait const[z] extends Function0 with Function1 with Function2 {
        override type apply0 = z
        override type apply1[+a] = z
        override type apply2[-a, +b] = z
    }

    // needed for variance-correct
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
    sealed trait List {
        type head[a]
        type tail <: List
    }

    trait cons[x[_], xs <: List] extends List {
        override type head[a] = x[a]
        override type tail = xs
    }

    trait nil extends List {
        override type head[a] = Nothing
        override type tail = Nothing
    }

    type list1[x1[_]] = cons[x1, nil]
    type list2[x1[_], x2[_]] = cons[x1, cons[x2, nil]]
    type list3[x1[_], x2[_], x3[_]] = cons[x1, cons[x2, cons[x3, nil]]]
    type list4[x1[_], x2[_], x3[_], x4[_]] = cons[x1, cons[x2, cons[x3, cons[x4, nil]]]]
    type list5[x1[_], x2[_], x3[_], x4[_], x5[_]] = cons[x1, cons[x2, cons[x3, cons[x4, cons[x5, nil]]]]]
    type list6[x1[_], x2[_], x3[_], x4[_], x5[_], x6[_]] = cons[x1, cons[x2, cons[x3, cons[x4, cons[x5, cons[x6, nil]]]]]]
    type list7[x1[_], x2[_], x3[_], x4[_], x5[_], x6[_], x7[_]] = cons[x1, cons[x2, cons[x3, cons[x4, cons[x5, cons[x6, cons[x7, nil]]]]]]]

    trait Contains[xs <: List, y[_]]

    object Contains {
        implicit def ofHead[x[_], xs <: List]: Contains[cons[x, xs], x] = new Contains[cons[x, xs], x] {}
        implicit def ofTail[x[_], xs <: List, y[_]](implicit ev: Contains[xs, y]): Contains[cons[x, xs], y] = new Contains[cons[x, xs], y] {}
    }
}
