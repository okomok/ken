

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
    class aliasOf(what: JString) extends StaticAnnotation

    /**
     * Method whose parameters are flipped from a method.
     */
    class flipOf(what: JString) extends StaticAnnotation

    /**
     * No special effects in Scala
     */
    class ceremonial(why: JString) extends StaticAnnotation

    /**
     * Unused name
     */
    class unused extends StaticAnnotation

    /**
     * Scalac is a good compiler.
     */
    class scalacWorkaround(version: JString, siNumber: Int = 0) extends StaticAnnotation

    /**
     * Without a named value in case class `copy`,
     * scalac complains "java.lang.Error: symbol value xxx does not exist".
     */
    class caseClassCopyWorkaround extends scalacWorkaround("2.9.1")

    /**
     * Avoid type-aliases. Scalac crashes unconditionally in user-site with:
     *   java.lang.IllegalArgumentException: transpose requires all collections have the same size
     */
    class typeAliasWorkaround extends scalacWorkaround("2.9.1")

    /**
     * Pending(unstable) feature
     */
    class pending(why: JString) extends StaticAnnotation
}
