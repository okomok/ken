
# ken 0.1.0-SNAPSHOT

`ken` is a Haskell DSL in Scala without any elaborate techniques:

    import com.github.okomok.ken.IO

    class ExampleTest extends org.scalatest.junit.JUnit3Suite {

        def teztIO {
            val io = for {
                x <- IO.getChar
                r <- IO.putChar(x)
            } yield r

            io.unIO()
        }
    }

The current status is pre-alpha.



## Links

* [Browse Source]
* [Browse Test Source]
* [The Scala Programming Language]


Shunsuke Sogame <<okomok@gmail.com>>



[MIT License]: http://www.opensource.org/licenses/mit-license.php "MIT License"
[Browse Source]: http://github.com/okomok/ken/tree/master/src/main/scala/ "Browse Source"
[Browse Test Source]: http://github.com/okomok/ken/tree/master/src/test/scala/ "Browse Test Source"
[The Scala Programming Language]: http://www.scala-lang.org/ "The Scala Programming Language"
