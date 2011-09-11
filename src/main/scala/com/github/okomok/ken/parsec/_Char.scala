

// Copyright Shunsuke Sogame 2011.
//
// (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package parsec


private[parsec] trait _Char[s, u, n[+_]] { this: _ParsecTs[s, u, n] =>
    def oneOf(cs: String)(implicit si: Stream[s, n, Char]): ParsecT[Char] = satisfy(c => List.elem(c)(cs))
    def noneOf(cs: String)(implicit si: Stream[s, n, Char]): ParsecT[Char] = satisfy(c => Bool.not(List.elem(c)(cs)))

    def spaces(implicit si: Stream[s, n, Char]): ParsecT[Unit] = skipMany(space) <#> "white space"

    def space(implicit si: Stream[s, n, Char]): ParsecT[Char] = satisfy(Char.isSpace) <#> "space"
    def newline(implicit si: Stream[s, n, Char]): ParsecT[Char] = char('\n') <#> "new-line"
    def tab(implicit si: Stream[s, n, Char]): ParsecT[Char] = char('\t') <#> "tab"

    def upper(implicit si: Stream[s, n, Char]): ParsecT[Char] = satisfy(Char.isUpper) <#> "uppercase letter"
    def lower(implicit si: Stream[s, n, Char]): ParsecT[Char] = satisfy(Char.isLower) <#> "lowercase letter"
    def alphaNum(implicit si: Stream[s, n, Char]): ParsecT[Char] = satisfy(Char.isAlphaNum) <#> "letter or digit"
    def letter(implicit si: Stream[s, n, Char]): ParsecT[Char] = satisfy(Char.isAlpha) <#> "letter"
    def digit(implicit si: Stream[s, n, Char]): ParsecT[Char] = satisfy(Char.isDigit) <#> "digit"
    def hexDigit(implicit si: Stream[s, n, Char]): ParsecT[Char] = satisfy(Char.isHexDigit) <#> "hexadecimal digit"
    def octDigit(implicit si: Stream[s, n, Char]): ParsecT[Char] = satisfy(Char.isOctDigit) <#> "octal digit"

    def char(c: Char)(implicit si: Stream[s, n, Char]): ParsecT[Char] = satisfy(_ == c) <#> Show.show(List(c))

    def anyChar(implicit si: Stream[s, n, Char]): ParsecT[Char] = satisfy(const(True))

    def satisfy(f: Char => Bool)(implicit si: Stream[s, n, Char]): ParsecT[Char] = {
        tokenPrim[Char, Char](c => Show.show(List(c)))(pos => c => _cs => updatePosChar(pos)(c))(c => if (f(c)) Just(c) else Nothing)
    }

    def string(s: String)(implicit si: Stream[s, n, Char]): ParsecT[String] = tokens[Char](Show.show)(updatePosString)(s)
}
