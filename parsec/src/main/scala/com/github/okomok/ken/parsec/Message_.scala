

// Copyright Shunsuke Sogame 2011.
//
// (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package parsec


sealed abstract class Message_ extends Up[Message_] {
    override def equals(that: Any): Boolean = that match {
        case that: Message_ => Message_.fromEnum(this) == Message_.fromEnum(that)
        case _ => false
    }
}

final case class SysUnExpect(s: String) extends Message_
final case class UnExpect(s: String) extends Message_
final case class Expect(s: String) extends Message_
final case class Message(s: String) extends Message_


object Message_ extends Enum[Message_] with Eq.Of[Message_] with Ord[Message_] with ThisIsInstance {
    // Overrides
    //
    // Enum
    override val fromEnum: fromEnum = {
        case SysUnExpect(_) => 0
        case UnExpect(_) => 1
        case Expect(_) => 2
        case Message(_) => 3
    }
    override val toEnum: toEnum = _ => error("toEnum is undefined for Message")
    // Ord
    override val compare: compare = m1 => m2 => {
        Int.compare(fromEnum(m1))(fromEnum(m2))
    }
}
