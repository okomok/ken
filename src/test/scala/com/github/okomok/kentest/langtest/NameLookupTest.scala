

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest.langtest


class NameLookupTest extends org.scalatest.junit.JUnit3Suite {


    object HasFoo {
        def foo = 10
    }

    def testNormal {
        trait A {
            def foo = 12
        }

        trait B extends A {
            def callFoo {
                import HasFoo._

                // ambiguous! because HasFoo.foo wins scope-race,
                // while A.foo wins precedence-race.
                //foo

                ()
            }
        }

        ()
    }


    class My
    class Your

    object PimpFoo {
        final class Foo(x: My) {
            def foo = 10
        }
        implicit def toFoo(x: My): Foo = new Foo(x)
    }

    def testEligible {
        trait A {
            final class Foo(x: Your) {
                def foo = 12
            }
            implicit def toFoo(x: Your): Foo = new Foo(x)
            //implicit def toFoo_(x: Your): Foo = new Foo(x) // wow
        }

        trait B extends A {
            def callFoo = {
                import PimpFoo._

                // should be ambiguous?
                //new My().foo

                //new Your().foo
                ()
            }
        }
    }
}
