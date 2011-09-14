

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


object Annotation {

    /**
     * Marker trait for documentation
     */
    class marker extends StaticAnnotation

    /**
     * Alias of a name
     */
    class aliasOf(what: String) extends StaticAnnotation

    /**
     * Method whose parameters are flipped from a method.
     */
    class flipOf(what: String) extends StaticAnnotation

}
