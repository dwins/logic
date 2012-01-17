package logic

import Symbolic.{ Sentence, Atom }, Sentential.symbolicSentences._, Ops._
import org.scalacheck._, Arbitrary.arbitrary

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
      case And(p, q) => Stream(p, q)
      case Or(p, q) => Stream(p, q)
      case ¬(p) => Stream(p)
      case Atom(_) => Stream(False, True)
      case _ => Stream.empty
    }
}
