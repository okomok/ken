

// Public domain


package com.github.okomok.kentest.langtest


// http://en.wikibooks.org/wiki/Haskell/Existentially_quantified_types


class QuantifiersTezt {

    class Cell[a](val elem: a) // nonvariant intentionally

    type Top = a forSome { type a }

    // Cell[_] =:= Cell[a] forSome { type a }
    // but Cell[_] =/= Cell[Top]

    val xs = new Cell[Top](3)
    val x: Top = xs.elem

    trait Bottom {
        def apply[a]: a
    }
}
