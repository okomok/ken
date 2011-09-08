

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


object Prim {

    // Exceptions
    //
    type IORep[a] = RealWorld.type => (a, RealWorld.type)

    def `catch`[a](io: IORep[a])(h: SomeException => IORep[a]): IORep[a] = { s =>
        try {
            io(s)
        } catch {
            case t: SomeException => h(t)(s)
        }
    }

    val raise: Throwable => Nothing = a => throw a

    val raiseIO: Throwable => IORep[Nothing] = a => s => (throw a, s)

}
