

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait Main {
    def main_ : IO[Any]

    protected def getArgs: IO[List[String_]] = IO.`return`(_args)

    private[this] var _args: List[String_] = null

    def main(args: Array[String]): Unit = {
        _args = args.map(str => List.from(str))
        main_.!
    }
}
