

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Functor {
    type f_[a]
    def fmap[a, b](x: a => b)(y: f_[a]): f_[b]

    def op_<#>[a, b](x: a => b)(y: f_[a]): f_[b] = fmap(x)(y)
    def op_<#[a, b](x: => a)(y: f_[b]): f_[a] = fmap((_: b) => x)(y)

    class Op_<#>[a, b](x: a => b) {
        def <#>(y: f_[a]): f_[b] = op_<#>(x)(y)
    }
    implicit def <#>[a, b](x: a => b): Op_<#>[a, b] = new Op_<#>(x)

    class Op_<#[a](x: => a) {
        def <#[b](y: f_[b]): f_[a] = op_<#(x)(y)
    }
    implicit def <#[a](x: => a): Op_<#[a] = new Op_<#(x)
}


object Functor {

    trait OfList extends Functor {
        override type f_[a] = List[a]
        override def fmap[a, b](x: a => b)(y: f_[a]): f_[b] = y match {
            case Nil => Nil
            case z :: zs => x(z) :: fmap(x)(zs)
        }
    }
    val OfList = new OfList {}

    trait OfOption extends Functor {
        override type f_[a] = Option[a]
        override def fmap[a, b](x: a => b)(y: f_[a]): f_[b] = y match {
            case None => None
            case Some(z) => Some(x(z))
        }
    }
    val OfOption = new OfOption {}

    trait OfFunction1[Z] extends Functor {
        override type f_[a] = Function1[Z, a]
        override def fmap[a, b](x: a => b)(y : f_[a]): f_[b] = z => x(y(z))
    }

}
