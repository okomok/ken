

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class KindTezt extends {

    implicitly[Kind.Contains[Num.type :^: Kind.nil, Num.type]]
    implicitly[Kind.Contains[Int :^: Num.type :^: Double :^: Kind.nil, Num.type]]
    implicitly[Kind.Contains[Num.type :^: String :^: Double :^: Kind.nil, Num.type]]
    implicitly[Kind.Contains[Int :^: Char :^: Num.type :^: Kind.nil, Num.type]]

}
