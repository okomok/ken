// Public domain

package com.github.okomok.kentest.example

import com.github.okomok.ken._

// http://www.cakesolutions.net/teamblogs/2011/09/16/kleisli-arrows/

class KleisliArrows extends org.scalatest.junit.JUnit3Suite {

    type DirName = String
    type File = JString

    lazy val files: DirName => List[File] = _ => List("my.txt", "your.txt")
    lazy val lengths: File => List[Int] = {
        case "my.txt" => List(1,3,7)
        case "your.txt" => List(13,20)
    }

    lazy val lineLengths: DirName => List[Int] = files >=>: lengths

    lazy val homeLineLength: DirName => List[Int] = ((home: DirName) => List[DirName]("/Users/" ++: home)) >=>: files >=>: lengths

    // Fortunate operator precedence: `>>=` < `>=>:`
    lazy val myLineLength: List[Int] = List[DirName]("/Users/janmachacek") >>= files >=>: lengths

    def testTrivial {
        expect(List(1,3,7,13,20))(lineLengths("dir"))
    }
}
