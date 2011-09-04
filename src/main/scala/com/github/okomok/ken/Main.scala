

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait Main {
    def main_ : IO[Any]

    protected def getArgs: IO[List[String]] = IO.unsafeIO(_args)

    private[this] var _args: List[String] = null

    def main(args: Array[String]): Unit = {
        _args = args
        main_.unIO()
    }
}
