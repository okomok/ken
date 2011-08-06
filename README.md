
# ken 0.1.0-SNAPSHOT

`ken` is a Haskell DSL in Scala without any elaborate technique:

    // From: http://en.wikibooks.org/wiki/Haskell/Monad_transformers

    import com.github.okomok.ken._

    // Strongly-typed monad; Haskell way.
    def testStrongMonadT {
        import IO.MaybeT

        // Pull the monad explicitly
        val m = MonadPlus[MaybeT.type]
        import m._

        val mt = MonadTrans[MaybeT.type]
        import mt.lift

        def isValid(s: String_): Boolean = Eq[String_].op_==(s)("valid")

        def getValidPassword: MaybeT[String_] = {
            for {
                s <- lift(IO.getLine)
                _ <- guard(isValid(s))
            } yield s
        }

        def askPassword: MaybeT[Unit] = for {
            _ <- lift(IO.putStrLn("Insert your new password"))
            value <- msum { List.repeat(getValidPassword) }
            _ <- lift(IO.putStrLn("Storing in database..."))
        } yield ()

        askPassword.run.unIO()
    }

    // Weakly-typed monad; No wrappers, no lifts.
    def testWeakMonadT {
        import IO.MaybeT

        val wm = MaybeT.weak.asMonadPlus
        import wm._

        def isValid(s: String_): Boolean = Eq[String_].op_==(s)("valid")

        def getValidPassword: IO[Maybe[String_]] = for {
            s <- IO.getLine
            _ <- guard(isValid(s))
        } yield s

        def askPassword: IO[Maybe[Unit]] = for {
            _ <- IO.putStrLn("Insert your new password")
            value <- msum { List.repeat(getValidPassword) }
            _ <- IO.putStrLn("Storing in database...")
        } yield Just()

        askPassword.unIO()
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
[Browse Source]: https://github.com/okomok/ken/tree/master/src/main/scala/com/github/okomok/ken "Browse Source"
[Browse Test Source]: https://github.com/okomok/ken/tree/master/src/test/scala/com/github/okomok/kentest "Browse Test Source"
[The Scala Programming Language]: http://www.scala-lang.org/ "The Scala Programming Language"
