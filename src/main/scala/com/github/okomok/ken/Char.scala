

// Copyright Shunsuke Sogame 2011.
//
// Copyright 2004, The University Court of the University of Glasgow.
// All rights reserved.
//
// Copyright (c) 2002 Simon Peyton Jones
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


import java.lang.{Character => JChar}


object Char extends Bounded[Char] with Enum[Char] with Eq.Of[Char] with Ord[Char] with Show.Of[Char] {
    // Overrides
    //
    // Bounded
    override val minBound: minBound = JChar.MIN_VALUE
    override val maxBound: maxBound = JChar.MAX_VALUE
    // Enum
    override val succ: succ = c => {
        if (not(ord(c) == maxBound)) chr(ord(c) + 1)
        else error("Enum[Char].succ: bad argument")
    }
    override val pred: pred = c => {
        if (not(ord(c) == minBound)) chr(ord(c) - 1)
        else error("Enum[Char].pred: bad argument")
    }
    override val toEnum: toEnum = chr
    override val fromEnum: fromEnum = ord
    // Ord
    override val op_< : op_< = c1 => c2 => c1 < c2
    override val op_<= : op_<= = c1 => c2 => c1 <= c2
    override val op_> : op_> = c1 => c2 => c1 > c2
    override val op_>= : op_>= = c1 => c2 => c1 >= c2

    // Utilities
    //
    val matches: String_ => Char => Bool = {
        str => ch => String.valueOf(ch).matches(List.stringize(str))
    }

    val isAscii: Char => Bool = ch => ch < 128
    val isLatin1: Char => Bool = ch => ch <= 255
    val isControl: Char => Bool = ch => JChar.isISOControl(ch)
    val isSpace: Char => Bool = ch => JChar.isSpaceChar(ch)
    val isLower: Char => Bool = ch => JChar.isLowerCase(ch)
    val isUpper: Char => Bool = ch => JChar.isUpperCase(ch)
    val isAlpha: Char => Bool = ch => JChar.isLetter(ch)
    val isAlphaNum: Char => Bool = ch => JChar.isLetterOrDigit(ch)
    val isPrint: Char => Bool = matches("\\p{Print}")
    val isDigit: Char => Bool = ch => JChar.isDigit(ch)

    val isOctDigit: Char => Bool = matches("[0-7]")
    val isHexDigit: Char => Bool = matches("[0-9]|[a-f]|[A-F]")

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
