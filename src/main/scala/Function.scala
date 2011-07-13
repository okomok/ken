

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok


object Function {
    def fix[a](f: (=> a) => a): a = {
        lazy val x: a = f(x)
        x
    }

    def fixfun[a](f: (a => a) => a => a): a => a = { x => f(fixfun(f))(x) }

    def on[a, b, c](* : b => b => c)(f: a => b): a => a => c = { x => y => *(f(x))(f(y)) }
}
