

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class IOTest extends org.scalatest.junit.JUnit3Suite {

    def teztIteract {
        val io = IO.interact { line =>
            List.foreach((ch: Char) => print(ch.toUpper))(line)
            line
        }

        io.unIO()
    }

    def teztGetLine {
        import Monad._
        val io = for (line <- IO.getLine) {
            IO.putStr(line)
        }
        io.unIO()
    }

    def testPrintLn {
        val io = IO.putStrLn("hello")
        /*
        io.unIO()
        io.unIO()
        io.unIO()
        */
    }
}
