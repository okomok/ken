

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait TypeClass0[+a] { // extends Kind.Function0 {
    type apply = a
}

trait TypeClass1[f[+_]] extends Kind.Function1 {
    override type apply[+a] = f[a]
}

trait TypeClass2[f[+_, +_]] extends Kind.Function2 {
    override type apply[+a, +b] = f[a, b]
}

trait TypeClass {
    // implicit def instance: this.type = this
}
