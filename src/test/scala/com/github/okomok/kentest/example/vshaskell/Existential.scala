// Public domain

package com.github.okomok.kentest.example.vshaskell

import com.github.okomok.ken._

class ExistentialTest extends org.scalatest.junit.JUnit3Suite {

    def testShow {
/*
        data Obj = forall a. (Show a) => Obj a

        xs :: [Obj]
        xs = [Obj 1, Obj "foo", Obj 'c']

        doShow :: [Obj] -> String
        doShow [] = ""
        doShow ((Obj x):xs) = show x ++ doShow xs
*/
        type ObjRep[a] = (a, Show[a])
        final case class Obj(rep: ObjRep[_]) {
            // SI-5022 workaround
            // Is there a better workaround?
            def apply[b](f: ObjRep[_] => b): b = f(rep)
        }
        object Obj_ { // A different name works around yet another bug in local case classes.
            def apply[a](a: a)(implicit s: Show[a]): Obj = new Obj(a, s)
        }

        val xs: List[Obj] = List(Obj_(1), Obj_("foo"), Obj_('c'))

        lazy val doShow: List[Obj] => String = {
            case Nil => ""
            // case Obj((x, s)) :: xs => show(x)(s) ++: doShow(xs)
            case obj :: xs => obj { case (x, s) => show(x)(s) } ++: doShow(xs)
        }

        expect("1fooc")(doShow(xs).asJString)
    }
}

// References
//
// http://www.haskell.org/haskellwiki/Existential_type

