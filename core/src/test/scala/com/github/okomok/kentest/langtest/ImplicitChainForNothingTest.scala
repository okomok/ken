

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest.langtest


class ImplicitChainForNothingTezt {

    trait Foo[x]

    object Foo {
        implicit val ofInt: Foo[Int] = new Foo[Int] {}
        implicit val ofUnit: Foo[Unit] = new Foo[Unit] {}
        implicit val ofNothing: Foo[Nothing] = new Foo[Nothing] {}
    }

    trait MyList[+a]

    object MyList {
        implicit def asFoo[a](implicit i: Foo[a]): Foo[MyList[a]] = new Foo[MyList[a]] {}
    }

    implicitly[Foo[Int]] // ok
    implicitly[Foo[Unit]] // ok
    implicitly[Foo[Nothing]] // ok
    implicitly[Foo[MyList[Int]]] // ok
    implicitly[Foo[MyList[Unit]]] // ok
    //implicitly[Foo[MyList[Nothing]]] // could not find implicit value for parameter e: ImplicitChainForNothingTezt.this.Foo[ImplicitChainForNothingTezt.this.MyList[Nothing]]

}

