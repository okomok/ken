

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class KindTezt extends {

    implicitly[Kind.MethodList.Contains[Kind.MethodList1[Num], Num]]
    implicitly[Kind.MethodList.Contains[Kind.MethodList3[Real, Num, Floating], Num]]
    implicitly[Kind.MethodList.Contains[Kind.MethodList3[Num, Real, Floating], Num]]
    implicitly[Kind.MethodList.Contains[Kind.MethodList3[Real, Floating, Num], Num]]

}
