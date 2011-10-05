

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken
package parsec


// type Parser[+a] = Parsec[String, Unit, a]


@Annotation.compilerWorkaround("2.9.1", 5031)
object _Parser extends Parsec.apply2[String, Unit] {
    def apply[a](n: UnParser[String, Unit, WeakIdentity.apply, a]): Parser[a] = Parsec(n)
    def unapply[a](m: Parser[a]): Option[UnParser[String, Unit, WeakIdentity.apply, a]] = Some(m.run)
}
