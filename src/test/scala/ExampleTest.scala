

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest


    import com.github.okomok.ken.IO

    class ExampleTest extends org.scalatest.junit.JUnit3Suite {

        def teztIO {
            val io = for {
                x <- IO.getChar
                r <- IO.putChar(x)
            } yield r

            io.unIO()
        }

        def test_ {}
    }
