

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest.example


    import com.github.okomok.ken._

    // From: http://www.haskell.org/haskellwiki/State_Monad
    object PlayGame extends Main {
        type GameValue = Int
        type GameState = (Boolean, Int)

        // Pulls the monad explicitly.
        val i = State.monad[GameState]
        import i._

        def playGame(xs: List[Char]): State[GameState, GameValue] = xs match {
            case Nil => for {
                (_, score) <- get
            } yield score
            case x :: xs => for {
                (on, score) <- get
                _ <- x match {
                    case 'a' if on => put(on, score + 1)
                    case 'b' if on => put(on, score - 1)
                    case 'c' => put(not(on), score)
                    case _ => put(on, score)
                }
                r <- playGame(xs.!)
            } yield r
        }

        val startState = (false, 0)

        val main_ = IO.print { State.eval(playGame("abcaaacbbcabbab"))(startState) }
    }


class PlayGameTest extends org.scalatest.junit.JUnit3Suite {
    def testTrivial {
        expect(2)(State.eval(PlayGame.playGame("abcaaacbbcabbab"))(PlayGame.startState))
    }
}
