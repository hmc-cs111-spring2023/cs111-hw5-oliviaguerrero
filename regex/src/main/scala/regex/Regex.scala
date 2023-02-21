package regex

/** *****************************************************************************
  * Regular Languages
  *
  * data structure definitions for regular languages
  */

trait RegularLanguage()

case object Empty   extends RegularLanguage
case object Epsilon extends RegularLanguage
case class Character(val c: Char) extends RegularLanguage
case class Union(val langs: (RegularLanguage, RegularLanguage))
                                                extends RegularLanguage
case class Concat(val langs: (RegularLanguage, RegularLanguage))
                                                extends RegularLanguage
case class Star(val lang: RegularLanguage)  extends RegularLanguage



/** *****************************************************************************
  * Derivatives
  *
  * Fill in the function definitions below
  */

/** Simplifies a regular language. Implemented Recursively. */
def simplify(lang: RegularLanguage): RegularLanguage =
  lang match
    case Union(langs) =>
      if      langs(0) == Empty then langs(1)
      else if langs(1) == Empty then langs(0) 
      else Union(simplify(langs(0)), simplify(langs(1)))

    case Concat(langs) => 
      if langs(0) == Empty || langs(1) == Empty then Empty
      else if langs(0) == Epsilon then langs(1)
      else if langs(1) == Epsilon then langs(0)
      else Concat(simplify(langs(0)), simplify(langs(1)))

    case Star(lang) =>
      if      lang == Empty   then  Empty
      else if lang == Epsilon then Epsilon
      else Star(simplify(lang))

    case _ => lang //For Empty/Epsilon/Char, return yourself

/** A language is nullable if it contains ε */
def nullable(lang: RegularLanguage): Boolean = ???

/** Computes the derivative of a language, with respect to a character */
def derivative(l: RegularLanguage)(c: Char): RegularLanguage = ???

/** *****************************************************************************
  * String-matching with regular expressions
  */

/** Given a string s and a language l, returns true if the string matches the
  * pattern described by l
  */
def matches(s: String, l: RegularLanguage): Boolean =
  if (s.isEmpty) then nullable(l)
  else matches(s.tail, derivative(l)(s.head))
