

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2000-2006, Koen Claessen
// Copyright (c) 2006, Bjorn Bringert
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package quickcheck


final case class Result(
    ok: Maybe[Bool],
    expect: Bool,
    reason: String,
    stamp: List[(String, Int)],
    callbacks: List[Callback]
)


object Result extends Testable[Result] {
    val ok: Result => Maybe[Bool] = res => res.ok
    val expect: Result => Bool = res => res.expect
    val reason: Result => String = res => res.reason
    val stamp: Result => List[(String, Int)] = res => res.stamp
    val callbacks: Result => List[Callback] = res => res.callbacks

    val result: Result = Result(null, True, "", Nil, Nil)
    val failed: Result = result.copy(ok = Just(False))

    def exception[a](err: a)(implicit i: Show[a]): Result = {
        @Annotation.caseClassCopyWorkaround
        val tmp = "Exception: \'" ++: Str.showErr(err) ++: List.from("\'")
        failed.copy(reason = tmp)
    }

    val succeeded: Result = result.copy(ok = Just(True))
    val rejected: Result = result.copy(ok = Nothing)

    // Overrides
    //
    // Testable
    override val property: property = x => Gen.`return`(Prop(Rose.`return`(IO.`return`(x))))
}
