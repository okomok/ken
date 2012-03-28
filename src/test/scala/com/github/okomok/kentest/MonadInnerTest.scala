

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


// A curious name resemblance to oven::adapted_to_base
class MonadInnerControlTest extends org.scalatest.junit.JUnit3Suite {

    def testInstanceSearch {
        type Env = String
        val _M = MonadReader[ReaderT.apply2[Env, ErrorT.apply2[String, WriterT.apply2[List[String], StateT.apply2[Int, IO.type]]]]]

        val _B1 = MonadInnerControl[IO.type, _M.type]
        val _B2 = MonadInnerControl[StateT.apply2[Int, IO.type], _M.type]
        val _B3 = MonadInnerControl[WriterT.apply2[List[String], StateT.apply2[Int, IO.type]], _M.type]
        val _B4 = MonadInnerControl[ErrorT.apply2[String, WriterT.apply2[List[String], StateT.apply2[Int, IO.type]]], _M.type]
        val _B5 = MonadInnerControl[ReaderT.apply2[Env, ErrorT.apply2[String, WriterT.apply2[List[String], StateT.apply2[Int, IO.type]]]], _M.type]
        val _B5_ = MonadInnerControl[_M.type, _M.type]
    }
}
