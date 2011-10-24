

// Copyright Shunsuke Sogame 2011.
//
// Copyright 2004, The University Court of the University of Glasgow.
// All rights reserved.
//
// Copyright (c) 2002 Simon Peyton Jones
//
// Distributed under the New BSD license.


// Draft


package com.github.okomok
package ken


trait Read[a] extends Typeclass[a] {
    final val asRead: Read[apply0] = this

    // Core
    //
    /*
    def readsPrec: Int => ReadS[a] = ReadPrec.readPrec_to_S(readPrec)
    def readList: ReadS[List[a]] = ReadPrec.readPrec_to_S(Read.list(readPrec))(0)

    // GHC proposals
    //
    def readPrec: ReadPrec[a] = ReadPrec.readS_to_Prec(readsPrec)
    def readListPrec: ReadPrec[List[a]] = ReadPrec.readS_to_Prec(_ => Read.readList)

    def readListDefault: ReadS[List[a]] = ReadPrec.readPrec_to_S(readListPrec)(0)
    def readListPrecDefault: ReadPrec[List[a]] = Read.list(readPrec)

    // Extra
    //
    def lex: ReadS[String] = Read.readP_to_S(Lex.hsLex)(s)

    def lexListChar: ReadS[JString] = Read.readP_to_S {
        for { (s, _) <- ReadP.gather(Lex.lexChar) } yield s
    }
    */

}


trait ReadProxy[a] extends Read[a] {
    def selfRead: Read[a]

}


object Read extends ReadInstance {
    def apply[a <: Kind.Function0](implicit i: Read[a#apply0]): Read[a#apply0] = i

}


sealed trait ReadInstance { this: Read.type =>
}
