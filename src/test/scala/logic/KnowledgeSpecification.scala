package logic

import org.scalacheck._
import Arbitrary.arbitrary, Prop.forAll

object KnowledgeSpecification extends Properties("Knowledge") {
  import Generators._
  import Knowledge.Oblivion._
  import Sentence._

  def atoms(s: Sentence): Set[Atom] = {
    def helper(s: Sentence, accum: Set[Atom]): Set[Atom] = 
      s match {
        case True | False => accum
        case p @ Atom(_) => accum + p
        case And(p, q) => helper(p, helper(q, accum))
        case Or(p, q) => helper(p, helper(q, accum))
        case ¬(p) => helper(p, accum)
      }

    helper(s, Set.empty)
  }

  def size(s: Sentence): Int = {
    def helper(s: Sentence, accum: Int): Int = 
      s match {
        case True | False | Atom(_) => accum + 1
        case And(p, q) => helper(p, helper(q, accum + 1))
        case Or(p, q) => helper(p, helper(q, accum + 1))
        case ¬(p) => helper(p, accum)
      }

    helper(s, 0)
  }

  property("Reduction never increases sentence size") = 
    forAll { (s: Sentence) => size(reduce(s)) <= size(s) }
  property("Reduction never introduces terms") =
    forAll { (s: Sentence) => atoms(reduce(s)) subsetOf atoms(s) }
  property("Conjunction with negation") =
    forAll { (s: Sentence) => reduce(s ∧ ¬(s)) == False }
  property("Disjunction with negation") =
    forAll { (s: Sentence) => reduce(s ∨ ¬(s)) == True }
  property("Conjunction with self") =
    forAll { (s: Sentence) => reduce(s ∧ s) == reduce(s) }
  property("Disjunction with self") =
    forAll { (s: Sentence) => reduce(s ∨ s) == reduce(s) }
}
