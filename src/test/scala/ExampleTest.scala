

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest


    import com.github.okomok.ken

    class ExampleTest extends org.scalatest.junit.JUnit3Suite {

        def teztIO {
            import ken.Monad.`for`

            val io = for {
                x <- ken.IO.getChar
            } {
                ken.IO.putChar(x)
            }

            io.unIO()
        }

        def test_ {}
    }
