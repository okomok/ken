

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2010, Bas van Dijk, Anders Kaseorg
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait MonadTrans[t[_[+_], +_]] extends TypeclassLike with Kind.MonadTrans { outer =>
    override type monadTrans[n[+_], +a] = t[n, a]
    final val asMonadTrans: MonadTrans[monadTrans] = this

    // Core
    //
    def lift[n[+_], a](n: n[a])(implicit _N: Monad[n]): t[n, a]

    // Default implementations
    //
    final def defaultMonadIO[n[+_]](_M: Monad[({type L[+a] = t[n, a]})#L], _N: MonadIO[n]): MonadIO[({type L[+a] = t[n, a]})#L] = new MonadIO[({type L[+a] = t[n, a]})#L] with MonadProxy[({type L[+a] = t[n, a]})#L] {
        private type m[+a] = t[n, a]
        override def selfMonad = _M
        override def liftIO[a](io: IO[a]): m[a] = outer.lift(_N.liftIO(io))(_N)
    }

    final def defaultMonadState[n[+_], s](_M: Monad[({type L[+a] = t[n, a]})#L], _N: MonadState[s, n]): MonadState[s, ({type L[+a] = t[n, a]})#L] = new MonadState[s, ({type L[+a] = t[n, a]})#L] with MonadProxy[({type L[+a] = t[n, a]})#L] {
        private type m[+a] = t[n, a]
        override def selfMonad = _M
        override val get: get = outer.lift(_N.get)(_N)
        override val put: put = s => outer.lift(_N.put(s))(_N)
    }
}


trait MonadTransProxy[t[_[+_], +_]] extends MonadTrans[t] {
    type selfMonadTrans = MonadTrans[t]
    def selfMonadTrans: selfMonadTrans
    override def lift[n[+_], a](n: n[a])(implicit _N: Monad[n]): t[n, a] = selfMonadTrans.lift(n)(_N)
}


object MonadTrans extends MonadTransInstance {
    def apply[t <: Kind.MonadTrans](implicit _T: MonadTrans[t#monadTrans]): MonadTrans[t#monadTrans] = _T
}


trait MonadTransInstance {
}
