

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
    def get: m[s]
    def put(s: s): m[Unit]

    // Extra
    //
    def modify(f: s => s): m[Unit] = for { s <- get; _ <- put(f(s)) } yield ()
    def gets[a](f: s => a): m[a] = for { s <- get } yield f(s)
}


trait MonadStateProxy[s, m[+_]] extends MonadState[s, m] with MonadProxy[m] {
    def selfMonadState: MonadState[s, m]
    override def selfMonad: Monad[m] = selfMonadState

    override def get: m[s] = selfMonadState.get
    override def put(s: s): m[Unit] = selfMonadState.put(s)

    override def modify(f: s => s): m[Unit] = selfMonadState.modify(f)
    override def gets[a](f: s => a): m[a] = selfMonadState.gets(f)
}


object MonadState extends MonadStateInstance {
    def apply[s, m <: Kind.Function1](implicit i: MonadState[s, m#apply]): MonadState[s, m#apply] = i

    def deriving[s, nt <: Kind.Newtype1](implicit j: Newtype1[nt#apply, nt#oldtype1], i: MonadState[s, nt#oldtype1]): MonadState[s, nt#apply] = new MonadState[s, nt#apply] with MonadProxy[nt#apply] {
        private type m[+a] = nt#apply[a]
        override val selfMonad = Monad.deriving[nt]

        override def get: m[s] = j.newOf { i.get }
        override def put(s: s): m[Unit] = j.newOf { i.put(s) }
    }

    def weak[s, nt <: Kind.Newtype1](implicit i: MonadState[s, nt#apply], j: Newtype1[nt#apply, nt#oldtype1]): MonadState[s, nt#oldtype1] = deriving[s, Kind.coNewtype1[nt]](j.coNewtype, i)
}


sealed trait MonadStateInstance { this: MonadState.type =>
}
