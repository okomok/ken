
# ken 0.1.0-SNAPSHOT

`ken` is a DSL like Haskell in Scala without any elaborate technique:

    import com.github.okomok.ken._

    // From: http://www.haskell.org/haskellwiki/State_Monad

    object StateGame extends Main {
        type GameValue = Int
        type GameState = (Bool, Int)

        // Pull the Monad explicitly.
        val _M = MonadState[State.apply[GameState]]
        import _M._

        val playGame: String => _M.apply[GameValue] = {
            case Nil => {
                for {
                    (_, score) <- get
                } yield score
            }
            case x :: xs => {
                for {
                    (on, score) <- get
                    _ <- x match {
                        case 'a' if on => put(on, score + 1)
                        case 'b' if on => put(on, score - 1)
                        case 'c' => put(Bool.not(on), score)
                        case _ => put(on, score)
                    }
                } playGame(xs)
            }
        }

        val startState = (False, 0)

        val main_ = IO.print { State.eval(playGame("abcaaacbbcabbab"))(startState) }
    }

The current status is alpha.



## Rationale

* Foolishly honest port of Haskell standard libraries.
* Variance-aware.
* Pulling type-class instances explicitly so that wildcard imports are removed.
* Type-level functions for partially-applied types.



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
