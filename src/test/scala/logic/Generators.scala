package logic

import Symbolic.{ Sentence, Atom }, Sentential.symbolicSentences._, Ops._
import org.scalacheck._, Arbitrary.arbitrary, Shrink.shrink

object Generators {
  implicit lazy val arbitrarySentence: Arbitrary[Sentence] = {
    val atoms = for (s <- Gen.oneOf('A, 'B, 'C)) yield Atom(s)

    val negations = 
      Gen.lzy { for (p <- arbitrary[Sentence]) yield ¬(p) }

    val conjunctions = 
      Gen.lzy {
        for {
          p <- arbitrary[Sentence]
          q <- arbitrary[Sentence]
        } yield (p ∧ q)
      }

    val disjunctions = 
      Gen.lzy {
        for {
          p <- arbitrary[Sentence]
          q <- arbitrary[Sentence]
        } yield (p ∨ q)
      }

    Arbitrary {
      Gen.frequency(
        (6, atoms),
        (1, negations),
        (1, conjunctions),
        (1, disjunctions)
      )
    }
  }

  implicit lazy val shrinkSentence: Shrink[Sentence] = 
    Shrink {
      case And(p, q) =>
        Stream(p, q) append
        (for { p2 <- shrink(p) ; p3 <- Seq(p2, And(p2, q)) } yield p3) append
        (for { q2 <- shrink(q) ; q3 <- Seq(q2, And(p, q2)) } yield q3)
      case Or(p, q) => Stream(p, q)
        Stream(p, q) append
        (for { p2 <- shrink(p) ; p3 <- Seq(p2, Or(p2, q)) } yield p3) append
        (for { q2 <- shrink(q) ; q3 <- Seq(q2, Or(p, q2)) } yield q3)
      case ¬(p) => Stream(p)
      case Atom(_) => Stream(False, True)
      case _ => Stream.empty
    }
}
