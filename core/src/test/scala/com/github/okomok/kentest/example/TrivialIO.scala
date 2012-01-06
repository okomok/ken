

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest.example


    import com.github.okomok.ken._

    object TrivialIO extends Main {
        val main_ = for { x <- IO.getChar
                          _ <- IO.putChar(x)
                    } yield ()
    }
