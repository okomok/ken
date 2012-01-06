

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


// A curious name resemblance to oven::adapted_to_base
class MonadBaseControlTest extends org.scalatest.junit.JUnit3Suite {

    def testInstanceSearch {
        type Env = String
        val _M = MonadReader[Env, ReaderT.apply2[Env, ErrorT.apply2[String, WriterT.apply2[List[String], StateT.apply2[Int, IO.type]]]]]

        val _B1 = MonadBaseControl[IO.type, _M.type]
        val _B2 = MonadBaseControl[StateT.apply2[Int, IO.type], _M.type]
        val _B3 = MonadBaseControl[WriterT.apply2[List[String], StateT.apply2[Int, IO.type]], _M.type]
        val _B4 = MonadBaseControl[ErrorT.apply2[String, WriterT.apply2[List[String], StateT.apply2[Int, IO.type]]], _M.type]
        val _B5 = MonadBaseControl[ReaderT.apply2[Env, ErrorT.apply2[String, WriterT.apply2[List[String], StateT.apply2[Int, IO.type]]]], _M.type]
        val _B5_ = MonadBaseControl[_M.type, _M.type]
    }
}
