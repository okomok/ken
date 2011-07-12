
# ken 0.1.0-SNAPSHOT

`ken` is a Haskell DSL in Scala without any elaborate techniques:

    import com.github.okomok.ken._

    object TrivialIO extends Main {
        val main_ = for { x <- IO.getChar
                          _ <- IO.putChar(x)
                    } yield ()
    }

The current status is pre-alpha.


## Links

* [ekmett/functorial - GitHub](https://github.com/ekmett/functorial "ekmett/functorial - GitHub")
* [Parsec](http://legacy.cs.uu.nl/daan/parsec.html "Parsec")
* [runarorama/scarpia - GitHub](https://github.com/runarorama/scarpia "runarorama/scarpia - GitHub")
* [scalaz](http://code.google.com/p/scalaz/ "scalaz")
* [Browse Source]
* [Browse Test Source]
* [The Scala Programming Language]


Shunsuke Sogame <<okomok@gmail.com>>


[MIT License]: http://www.opensource.org/licenses/mit-license.php "MIT License"
[Browse Source]: http://github.com/okomok/ken/tree/master/src/main/scala/ "Browse Source"
[Browse Test Source]: http://github.com/okomok/ken/tree/master/src/test/scala/ "Browse Test Source"
[The Scala Programming Language]: http://www.scala-lang.org/ "The Scala Programming Language"
