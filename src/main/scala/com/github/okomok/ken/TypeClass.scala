

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait TypeClass0[a] extends Kind.Function0 {
    override type apply = a
    final def infer(x: a): a = x // unused
}

trait TypeClass1[f[+_]] extends Kind.Function1 {
    override type apply[+a] = f[a]
    final def infer[a](x: f[a]): f[a] = x
}

trait TypeClass2[f[+_, +_]] extends Kind.Function2 {
    override type apply[+a, +b] = f[a, b]
    final def infer[a, b](x: f[a, b]): f[a, b] = x
}

trait TypeClass {
    // implicit def instance: this.type = this
}
