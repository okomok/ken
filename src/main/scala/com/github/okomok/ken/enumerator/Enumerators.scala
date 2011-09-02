

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken
package enumerator


final class Enumerators[n <: Kind.Function1](override implicit val inner: Monad[n#apply]) extends EnumeratorsBase[n]

trait EnumeratorsBase[n <: Kind.Function1] extends _Enumerators[n#apply]


private[enumerator] trait _Enumerators[n[+_]] extends MonadTs[n]
    with _Primitives[n] with _ListAnalogues[n] with _Types[n] with _Utilities[n]
