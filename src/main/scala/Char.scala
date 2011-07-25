

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


import java.lang.{Character => JChar}


object Char {
    val matches: String_ => Char => Bool = {
        str => ch => String.valueOf(ch).matches(List.stringize(str))
    }

    val isAscii: Char => Bool = { ch => ch < 128 }
    val isLatin1: Char => Bool = { ch => ch <= 255 }
    val isControl: Char => Bool = { ch => JChar.isISOControl(ch) }
    val isSpace: Char => Bool = { ch => JChar.isSpaceChar(ch) }
    val isLower: Char => Bool = { ch => JChar.isLowerCase(ch) }
    val isUpper: Char => Bool = { ch => JChar.isUpperCase(ch)  }
    val isAlpha: Char => Bool = { ch => JChar.isLetter(ch) }
    val isAlphaNum: Char => Bool = { ch => JChar.isLetterOrDigit(ch) }
    val isPrint: Char => Bool = matches("\\p{Print}")
    val isDigit: Char => Bool = { ch => JChar.isDigit(ch) }

    val isOctDigit: Char => Bool = matches("[0-7]")
    val isHexDigit: Char => Bool = matches("[0-9]|[a-f]|[A-F]")

    val toUpper: Char => Char = { ch => JChar.toUpperCase(ch) }
    val toLower: Char => Char = { ch => JChar.toLowerCase(ch) }

    val digitToInt: Char => Int = { ch => JChar.getNumericValue(ch) }
    val intToDigit: Int => Char = {
        case 0 => '0'
        case 1 => '1'
        case 2 => '2'
        case 3 => '3'
        case 4 => '4'
        case 5 => '5'
        case 6 => '6'
        case 7 => '7'
        case 8 => '8'
        case 9 => '9'
        case 10 => 'a'
        case 11 => 'b'
        case 12 => 'c'
        case 13 => 'd'
        case 14 => 'e'
        case 15 => 'f'
    }

    val ord: Char => Int = { ch => ch.toInt }
    val chr: Int => Char = { n => n.toChar }

    //val showLitChar: Char => Show.ShowS

    //val lexLitChar: Read.ReadS[String_]
    //val readLitChar: Read.ReadS[Char]
}
