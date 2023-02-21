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
case class Union(val lang1: RegularLanguage, val lang2: RegularLanguage)
                                                extends RegularLanguage
case class Concat(val lang1: RegularLanguage, val lang2: RegularLanguage)
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
    case Union(lang1, lang2) =>
      if      lang1 == Empty then lang2
      else if lang2 == Empty then lang1 
      else Union(simplify(lang1), simplify(lang2))

    case Concat(lang1, lang2) => 
      if lang1 == Empty || lang2 == Empty then Empty
      else if lang1 == Epsilon then lang2
      else if lang2 == Epsilon then lang1
      else Concat(simplify(lang1), simplify(lang2))

    case Star(subLang) =>
      if      subLang == Empty   then  Empty
      else if subLang == Epsilon then Epsilon
      else Star(simplify(subLang))

    case _ => lang //For Empty/Epsilon/Char, return yourself

/** A language is nullable if it contains ε */
def nullable(lang: RegularLanguage): Boolean = 
  lang match
    // If you need to, look recursively.
    case Union(lang1, lang2)  => nullable(lang1) || nullable (lang2)
    case Concat(lang1, lang2) => nullable(lang1) && nullable (lang2)
    // Otherwise, decide based on the current class.
    case Star(subLang)    => true
    case Epsilon    => true
    case _          => false //Both empty and char should return false

/** Computes the derivative of a language, with respect to a character */
def derivative(l: RegularLanguage)(c: Char): RegularLanguage = 
  l match
    case Concat(lang1, lang2) =>
      val restOfL = Concat(derivative(lang1)(c), lang2)
      if nullable(lang1) then Union(restOfL, derivative(lang2)(c))
      else restOfL
    case Union(lang1, lang2) => Union( derivative(lang1)(c),
                                       derivative(lang2)(c))
    case Star(subLang) => Concat(derivative(subLang)(c), Star(subLang))
    case Character(charC) => if c == charC then Epsilon else Empty
    case _ => Empty // Both Empty and Epsilon return Empty
  

/** *****************************************************************************
  * String-matching with regular expressions
  */

/** Given a string s and a language subLang, returns true if the string matches the
  * pattern described by subLang
  */
def matches(s: String, subLang: RegularLanguage): Boolean =
  if (s.isEmpty) then nullable(subLang)
  else matches(s.tail, derivative(subLang)(s.head))
