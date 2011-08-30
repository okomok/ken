

// Copyright Shunsuke Sogame 2011.
//
// (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package parsec


private[ken] trait _Error[n[+_]] { this: _Parsec[n] =>
    // Message
    //
    sealed abstract class Message_ extends Up[Message_] {
        override def equals(that: Any): Boolean = that match {
            case that: Message_ => Message_.fromEnum(this) == Message_.fromEnum(that)
            case _ => false
        }
    }

    final case class SysUnExpect(s: String_) extends Message_
    final case class UnExpect(s: String_) extends Message_
    final case class Expect(s: String_) extends Message_
    final case class Message(s: String_) extends Message_

    object Message_ extends Enum[Message_] with Eq.Of[Message_] with Ord[Message_] with ThisIsInstance {
        // Overrides
        //
        // Enum
        private type a = Message_
        override val fromEnum: a => Int = {
            case SysUnExpect(_) => 0
            case UnExpect(_) => 1
            case Expect(_) => 2
            case Message(_) => 3
        }
        override val toEnum: Int => a = error("toEnum is undefined for Message")
        // Ord
        override val compare: a => a => Ordering = m1 => m2 => {
            Ord[Kind.const[Int]].compare(fromEnum(m1))(fromEnum(m2))
        }
    }

    val messageString: Message_ => String_ = {
        case SysUnExpect(s) => s
        case UnExpect(s) => s
        case Expect(s) => s
        case Message(s) => s
    }

    // ParseError
    //
    final case class ParseError(pos: SourcePos, msgs: List[Message_]) {
        override def toString: String = List.stringize(ParseError.show(this))
    }

    object ParseError extends Eq.Of[ParseError] with Show[ParseError] with ThisIsInstance {
        // Overrides
        //
        // Show
        private type a = ParseError
        override val show: a => String_ = err => {
            ken.show(errorPos(err)) ::: ":" :::
                showErrorMessages("or")("unknown parse error")("expecting")("unexpected")("end of input")(errorMessages(err))
        }
    }

    val errorPos: ParseError => SourcePos = err => err.pos
    val errorMessages: ParseError => List[Message_] = err => List.sort(err.msgs)
    val errorIsUnknown: ParseError => Bool = err => List.`null`(err.msgs)

    // Create parse errors
    //
    val newErrorUnknown: SourcePos => ParseError = pos => ParseError(pos, Nil)
    val newErrorMessage: Message_ => SourcePos => ParseError = msg => pos => ParseError(pos, List(msg))
    val addErrorMessage: Message_ => ParseError => ParseError = msg => err => err.copy(msgs = msg :: err.msgs)
    val setErrorPos: SourcePos => ParseError => ParseError = pos => err => err.copy(pos = pos)
    val setErrorMessage: Message_ => ParseError => ParseError = msg => err => err.copy(msgs = msg :: List.filter(msg /== (_: Message_))(err.msgs))
    val mergeError: ParseError => ParseError => ParseError = err1 => err2 => ParseError(err1.pos, err1.msgs ::: err2.msgs)

    // Language independent show function
    //
    private[ken] def showErrorMessages(msgOr: String_)
        (msgUnknown: String_)(msgExpecting: String_)(msgUnExpected: String_)
        (msgEndOfInput: String_)(msgs: List[Message_]): String_ =
    {
        val (sysUnExpect, msgs1) = List.span(SysUnExpect("") === (_: Message_))(msgs)
        val (unExpect, msgs2) = List.span(UnExpect("") === (_: Message_))(msgs1)
        val (expect, messages) = List.span(Expect("") === (_: Message_))(msgs2)

        def clean(ms: List[String_]): List[String_] = List.nub(List.filter[String_](not compose List.`null`)(ms))

        def separate(sep: String_)(ms: List[String_]): String_ = ms match {
            case Nil => Nil
            case m !:: Nil => m
            case m :: ms => m ::: sep ::: separate(sep)(ms.!)
        }

        def commasOr(s: List[String_]): String_ = s match {
            case Nil => Nil
            case m !:: Nil => m
            case ms => commaSep(List.init(ms)) ::: " " ::: msgOr ::: " " ::: List.last(ms)
        }

        def commaSep(ms: List[String_]): String_ = separate(", ")(clean(ms))
        def semiSep(ms: List[String_]): String_ = separate("; ")(clean(ms))

        def showMany(pre: String_)(msgs: List[Message_]): String_ = {
            clean(List.map(messageString)(msgs)) match {
                case Nil => ""
                case ms => {
                    if (List.`null`(pre)) commasOr(ms)
                    else pre ::: " " ::: commasOr(ms)
                 }
            }
        }

        def showExpect: String_ = showMany(msgExpecting)(expect)
        def showUnExpect: String_ = showMany(msgUnExpected)(unExpect)
        def showSysUnExpect: String_ = {
            def firstMsg: String_ = messageString(List.head(sysUnExpect))

            if (not(List.`null`(unExpect)) || List.`null`(sysUnExpect)) ""
            else if (List.`null`(firstMsg)) msgUnExpected ::: " " ::: msgEndOfInput
            else msgUnExpected ::: " " ::: firstMsg
        }
        def showMessages: String_ = showMany("")(messages)

        if (List.`null`(msgs)) {
            msgUnknown
        } else {
            List.concat { List.map((m: String_) => "\n" ::: m) { clean {
                List(showSysUnExpect, showUnExpect, showExpect, showMessages)
            } } }
        }
    }
}
