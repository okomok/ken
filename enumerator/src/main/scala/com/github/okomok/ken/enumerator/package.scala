

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2010 John Millikin
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


// enumerator: https://john-millikin.com/software/enumerator/


package object enumerator {

    // types
    //
    type Enumerator[a, n[+_], b] = Step[a, n, b] => Iteratee[a, n, b]
    val Enumerator = _Enumerator

    type Enumeratee[ao, ai, n[+_], b] = Step[ai, n, b] => Iteratee[ao, n, Step[ai, n, b]]
}
