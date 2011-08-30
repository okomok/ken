

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


// enumerator: https://john-millikin.com/software/enumerator/


import enumerator._


final class Enumerators[n <: Kind.Function1](override implicit val inner: Monad[n#apply]) extends EnumeratorsOf[n]

trait EnumeratorsOf[n <: Kind.Function1] extends _Enumerators[n#apply]


private[ken] trait _Enumerators[n[+_]]
    extends _ListAnalogues[n] with _Primitives[n] with _Types[n] with _Utilities[n]
{
    val inner: Monad[n]
}
