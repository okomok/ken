

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


import scala.annotation.unchecked.uncheckedVariance


trait Typeclass[-a] extends TypeclassLike with Type[a @uncheckedVariance]
