

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


import java.lang.{Character => JChar}


object Char {
    val isAscii: Char => Boolean = ch => ch < 128
    val isLatin1: Char => Boolean = ch => ch <= 255
    val isControl: Char => Boolean = ch => JChar.isISOControl(ch)
    val isSpace: Char => Boolean = ch => JChar.isSpaceChar(ch)
    val isLower: Char => Boolean = ch => JChar.isLowerCase(ch)
    val isUpper: Char => Boolean = ch => JChar.isUpperCase(ch)
    val isAlpha: Char => Boolean = ch => JChar.isLetter(ch)
    val isAlphaNum: Char => Boolean = ch => JChar.isLetterOrDigit(ch)
    val isPrint: Char => Boolean = ch => false
    val isDigit: Char => Boolean = ch => JChar.isDigit(ch)

    val isOctDigit: Char => Boolean = ch => String.valueOf(ch).matches("[0-7]")
    val isHexDigit: Char => Boolean = ch => String.valueOf(ch).matches("[0-9]|[a-f]|[A-F]")

    val toUpper: Char => Char = ch => JChar.toUpperCase(ch)
    val toLower: Char => Char = ch => JChar.toLowerCase(ch)

    val digitToInt: Char => Int = ch => JChar.getNumericValue(ch)
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

    val ord: Char => Int = ch => ch.toInt
    val chr: Int => Char = n => n.toChar

    //val showLitChar: Char => Show.ShowS

    //val lexLitChar: Read.ReadS[String_]
    //val readLitChar: Read.ReadS[Char]
}
