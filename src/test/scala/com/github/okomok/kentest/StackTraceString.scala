

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


object StackTraceString {
    def apply(): String = {
        var that: String = null
        try {
            throw new RuntimeException("StackTraceString")
        } catch {
            case t: Throwable => that = fromThrowable(t)
        }
        that
    }

    def fromThrowable(t: Throwable): String = {
        val w = new java.io.StringWriter()
        t.printStackTrace(new java.io.PrintWriter(w))
        w.toString
    }
}
