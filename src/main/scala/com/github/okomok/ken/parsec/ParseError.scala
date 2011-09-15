

// Copyright Shunsuke Sogame 2011.
//
// (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package parsec


final case class ParseError(pos: SourcePos, msgs: List[Message_]) {
    override def toString: JString = List.toJString(ParseError.show(this))
}


object ParseError extends Eq.Of[ParseError] with Show[ParseError] with ThisIsInstance {
    // Overrides
    //
    // Show
    override val show: show = err => {
        Show.show(errorPos(err)) ++: ":" ++:
            showErrorMessages("or")("unknown parse error")("expecting")("unexpected")("end of input")(errorMessages(err))
    }

    // Language independent show function
    //
    private def showErrorMessages(msgOr: String)
        (msgUnknown: String)(msgExpecting: String)(msgUnExpected: String)
        (msgEndOfInput: String)(msgs: List[Message_]): String =
    {
        val (sysUnExpect, msgs1) = List.span(SysUnExpect("") === (_: Message_))(msgs)
        val (unExpect, msgs2) = List.span(UnExpect("") === (_: Message_))(msgs1)
        val (expect, messages) = List.span(Expect("") === (_: Message_))(msgs2)

        def clean(ms: List[String]): List[String] = List.nub(List.filter[String](Bool.not `.` List.`null`)(ms))

        def separate(sep: String)(ms: List[String]): String = ms match {
            case Nil => Nil
            case m !:: Nil => m
            case m :: ms => m ++: sep ++: separate(sep)(ms.!)
        }

        def commasOr(s: List[String]): String = s match {
            case Nil => Nil
            case m !:: Nil => m
            case ms => commaSep(List.init(ms)) ++: " " ++: msgOr ++: " " ++: List.last(ms)
        }

        def commaSep(ms: List[String]): String = separate(", ")(clean(ms))
        def semiSep(ms: List[String]): String = separate("; ")(clean(ms))

        def showMany(pre: String)(msgs: List[Message_]): String = {
            clean(List.map(messageString)(msgs)) match {
                case Nil => ""
                case ms => {
                    if (List.`null`(pre)) commasOr(ms)
                    else pre ++: " " ++: commasOr(ms)
                 }
            }
        }

        def showExpect: String = showMany(msgExpecting)(expect)
        def showUnExpect: String = showMany(msgUnExpected)(unExpect)
        def showSysUnExpect: String = {
            def firstMsg: String = messageString(List.head(sysUnExpect))

            if (Bool.not(List.`null`(unExpect)) || List.`null`(sysUnExpect)) ""
            else if (List.`null`(firstMsg)) msgUnExpected ++: " " ++: msgEndOfInput
            else msgUnExpected ++: " " ++: firstMsg
        }
        def showMessages: String = showMany("")(messages)

        if (List.`null`(msgs)) {
            msgUnknown
        } else {
            List.concat { List.map((m: String) => "\n" ++: m) { clean {
                List(showSysUnExpect, showUnExpect, showExpect, showMessages)
            } } }
        }
    }
}
