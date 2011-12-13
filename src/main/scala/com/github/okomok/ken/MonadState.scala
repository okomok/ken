

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


trait MonadState[s, m[+_]] extends Monad[m] {
    final val asMonadState: MonadState[s, apply] = this

    // Core
    //
    type get = m[s]
    def get: get

    type put = s => m[Unit]
    def put: put

    // Extra
    //
    type modify = (s => s) => m[Unit]
    def modify: modify = f => for { s <- get; _ <- put(f(s)) } yield ()

    def gets[a](f: s => a): m[a] = for { s <- get } yield f(s)
}


trait MonadStateProxy[s, m[+_]] extends MonadState[s, m] with MonadProxy[m] {
    def selfMonadState: MonadState[s, m]
    override def selfMonad: Monad[m] = selfMonadState

    override def get: get = selfMonadState.get
    override def put: put = selfMonadState.put

    override def modify: modify = selfMonadState.modify
    override def gets[a](f: s => a): m[a] = selfMonadState.gets(f)
}


object MonadState extends MonadStateInstance {
    def apply[s, m <: Kind.Function1](implicit i: MonadState[s, m#apply1]): MonadState[s, m#apply1] = i

    def deriving[s, nt <: Kind.Newtype1](implicit j: Newtype1[nt#apply1, nt#oldtype1], i: MonadState[s, nt#oldtype1]): MonadState[s, nt#apply1] = new MonadState[s, nt#apply1] with MonadProxy[nt#apply1] {
        private type m[+a] = nt#apply1[a]
        override val selfMonad = Monad.deriving[nt]

        override val get: get = j.newOf { i.get }
        override val put: put = s => j.newOf { i.put(s) }
    }

    def weak[s, nt <: Kind.Newtype1](implicit i: MonadState[s, nt#apply1], j: Newtype1[nt#apply1, nt#oldtype1]): MonadState[s, nt#oldtype1] = deriving[s, Kind.coNewtype1[nt]](j.coNewtype, i)
}


sealed trait MonadStateInstance { this: MonadState.type =>
}
