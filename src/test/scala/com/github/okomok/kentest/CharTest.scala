

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class CharTest extends org.scalatest.junit.JUnit3Suite {

    def testTrivial {
        expect(65)(Char.ord('A'))
        expect('A')(Char.chr(65))
        expect(10)(Char.digitToInt('A'))
        expect(10)(Char.digitToInt('a'))
        assert(Char.isPrint('a'))
    }
}
