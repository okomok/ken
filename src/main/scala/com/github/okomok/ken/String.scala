

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


// type String = List[Char]


private[ken] object _String extends (JString => String) {
    override def apply(str: JString): String = List.from(str)
    def unapply(str: String): Option[JString] = Some(str.asJString)
}
