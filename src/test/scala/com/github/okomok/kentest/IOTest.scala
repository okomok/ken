

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class IOTest extends org.scalatest.junit.JUnit3Suite {

    def teztIteract {
        val io = IO.interact { line =>
            line.foreach((ch: Char) => print(ch.toUpper))
            line
        }

        io.!
    }

    def teztGetLine {
        val io = for {
            line <- IO.getLine
            * <- IO.putStr(line)
        } yield *

        io.!
    }

    def teztPrintLn {
        val io = IO.putStrLn("hello")

        //io.!
        //io.!
        //io.!
    }

    def test_ {}
}
