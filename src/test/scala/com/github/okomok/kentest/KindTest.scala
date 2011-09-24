

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class KindTezt extends {

    implicitly[Kind.Contains[Num :^: Kind.nil, Num]]
    implicitly[Kind.Contains[Real :^: Num :^: Floating :^: Kind.nil, Num]]
    implicitly[Kind.Contains[Num :^: Real :^: Floating :^: Kind.nil, Num]]
    implicitly[Kind.Contains[Real :^: Floating :^: Num :^: Kind.nil, Num]]

}
