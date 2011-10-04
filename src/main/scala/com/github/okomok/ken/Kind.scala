

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

    trait MonadTransX {
        type monadTrans[m[+_], +a]
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
    sealed trait ^:[x, xs <: List] extends List
    sealed trait Nil extends List with MethodList

    object List {
        trait Contains[xs <: List, y]

        object Contains {
            implicit def ofHead[x, xs <: List]: Contains[x ^: xs, x] = new Contains[x ^: xs, x] {}
            implicit def ofTail[x, xs <: List, y](implicit ev: Contains[xs, y]): Contains[x ^: xs, y] = new Contains[x ^: xs, y] {}
        }
    }

    // MethodList (for cute syntax, but may be rejected.)
    //
    sealed trait MethodList
    sealed trait ^::[f[_], fs <: MethodList] extends MethodList

    object MethodList {
        trait Contains[fs <: MethodList, g[_]]

        object Contains {
            implicit def ofHead[f[_], fs <: MethodList]: Contains[f ^:: fs, f] = new Contains[f ^:: fs, f] {}
            implicit def ofTail[f[_], fs <: MethodList, g[_]](implicit ev: Contains[fs, g]): Contains[f ^:: fs, g] = new Contains[f ^:: fs, g] {}
        }
    }
}
