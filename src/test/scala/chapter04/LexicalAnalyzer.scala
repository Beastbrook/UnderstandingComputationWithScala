package uc.lexical

import scala.util.matching.Regex

object LexicalAnalyzer {

  val GRAMMAR: List[(Char, Regex)] = List(
    ('i', """\A(if)(?![a-z0-9])""".r),
    ('e', """\A(else)(?![a-z0-9])""".r),
    ('w', """\A(while)(?![a-z0-9])""".r),
    ('d', """\A(do-nothing)(?![a-z0-9])""".r),
    ('(', """\A\(""".r),
    (')', """\A\)""".r),
    ('{', """\A\{""".r),
    ('}', """\A\}""".r),
    (';', """\A;""".r),
    ('=', """\A=""".r),
    ('+', """\A\+""".r),
    ('*', """\A\*""".r),
    ('<', """\A<""".r),
    ('n', """\A[0-9]+""".r),
    ('b', """\A(true|false)(?![a-z0-9])""".r),
    ('v', """\A[a-z]+""".r)
  )

  def moreTokens(string: String): Boolean =
    string != ""

  def nextStep(string: String): (Char, String) = {
    val rule: (Char, Regex) = ruleFor(string.trim)
    (rule._1, rule._2.replaceFirstIn(string.trim, ""))
  }

  def ruleFor(string: String): (Char, Regex) =
    GRAMMAR.find(rule => rule._2.findFirstIn(string).getOrElse("") != "").get

  def analyze(string: String): List[Char] =
    if (string == "") Nil
    else {
      val next: (Char, String) = nextStep(string)
      next._1 :: analyze(next._2)
    }

}