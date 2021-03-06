

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest.example


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


class StateGameTest extends org.scalatest.junit.JUnit3Suite {
    def testTrivial {
        expect(2)(State.eval(StateGame.playGame("abcaaacbbcabbab"))(StateGame.startState))
    }
}
