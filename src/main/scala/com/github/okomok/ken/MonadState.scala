

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


trait MonadState[m[+_]] extends Monad[m] {
    final val asMonadState: MonadState[apply1] = this

    // Core
    //
    type StateType

    type get = m[StateType]
    def get: get

    type put = StateType => m[Unit]
    def put: put

    // Extra
    //
    type modify = (StateType => StateType) => m[Unit]
    def modify: modify = f => for { s <- get; _ <- put(f(s)) } yield ()

    def gets[a](f: StateType => a): m[a] = for { s <- get } yield f(s)
}


trait MonadStateProxy[m[+_]] extends MonadState[m] with MonadProxy[m] {
    type selfMonadState = MonadState[m]
    val selfMonadState: selfMonadState
    override def selfMonad: selfMonad = selfMonadState

    override type StateType = selfMonadState.StateType
    override def get: get = selfMonadState.get
    override def put: put = selfMonadState.put

    override def modify: modify = selfMonadState.modify
    override def gets[a](f: StateType => a): m[a] = selfMonadState.gets(f)
}


object MonadState extends MonadStateInstance {
    type Of[s, m[+a]] = MonadState[m] { type StateType = s }

    def apply[m <: Kind.Function1](implicit _M: MonadState[m#apply1]): Of[_M.StateType, m#apply1] = _M

    def deriving[nt <: Kind.Newtype1](implicit _Nt: Newtype1[nt#apply1, nt#oldtype1], _M: MonadState[nt#oldtype1]): Of[_M.StateType, nt#apply1] = new MonadState[nt#apply1] with MonadProxy[nt#apply1] {
        private type m[+a] = nt#apply1[a]
        override val selfMonad: selfMonad = Monad.deriving[nt]

        override type StateType = _M.StateType
        override val get: get = _Nt.newOf { _M.get }
        override val put: put = s => _Nt.newOf { _M.put(s) }
    }

    def weak[nt <: Kind.Newtype1](implicit _M: MonadState[nt#apply1], _Nt: Newtype1[nt#apply1, nt#oldtype1]): Of[_M.StateType, nt#oldtype1] = deriving[Kind.coNewtype1[nt]](_Nt.coNewtype, _M)
}


sealed trait MonadStateInstance { this: MonadState.type =>
}
