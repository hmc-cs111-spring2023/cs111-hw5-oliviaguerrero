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
case class Union(val left: RegularLanguage, val right: RegularLanguage)
                                                extends RegularLanguage
case class Concat(val first: RegularLanguage, val rest: RegularLanguage)
                                                extends RegularLanguage
case class Star(val repeater: RegularLanguage)  extends RegularLanguage



/** *****************************************************************************
  * Derivatives
  *
  * Fill in the function definitions below
  */

/** Simplifies a regular language */
def simplify(lang: RegularLanguage): RegularLanguage = ???

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
