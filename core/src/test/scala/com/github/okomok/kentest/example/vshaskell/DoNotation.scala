// Public domain

package com.github.okomok.kentest.example.vshaskell

import com.github.okomok.ken._

class DoNotationTest extends org.scalatest.junit.JUnit3Suite {

    def testReturn {
/*
        a = do x <- [3..4]
               [1..2]
               return (x, 42)
*/
        val a = for {
            x <- Int.enumFromTo(3)(4)
            _ <- Int.enumFromTo(1)(2)
        } yield (x, 42)

        expect(List((3,42),(3,42),(4,42),(4,42)))(a)
    }

    def testNoReturn {
/*
        nameDo :: IO ()
        nameDo = do putStr "What is your first name? "
                    first <- getLine
                    putStr "And your last name? "
                    last <- getLine
                    let full = first++" "++last
                    putStrLn ("Pleased to meet you, "++full++"!")
*/
        val nameDo: IO[Unit] = {
            for {
                _ <- IO.putStr("What is your first name? ")
                first <- IO.getLine
                _ <- IO.putStr("And your last name? ")
                last <- IO.getLine
                full = first ++: " " ++: last
            }   IO.putStrLn("Pleased to meet you, " ++: full ++: List.from("!"))
        }

        ignore {
            nameDo.! // Don't forget the evaluation!
        }
    }
}

// References
//
// http://en.wikibooks.org/wiki/Haskell/do_Notation
// http://en.wikipedia.org/wiki/Monad_%28functional_programming%29#do-notation

