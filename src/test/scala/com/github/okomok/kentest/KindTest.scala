

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class KindTezt extends {

    implicitly[Kind.MethodList.Contains[Num ^:: Kind.Nil, Num]]
    implicitly[Kind.MethodList.Contains[Real ^:: Num ^:: Floating ^:: Kind.Nil, Num]]
    implicitly[Kind.MethodList.Contains[Num ^:: Real ^:: Floating ^:: Kind.Nil, Num]]
    implicitly[Kind.MethodList.Contains[Real ^:: Floating ^:: Num ^:: Kind.Nil, Num]]

}
