package dwins.logic

import org.scalacheck._, Arbitrary._, Prop._

object KnowledgeSpecification extends Properties("Knowledge") {
  import Generators._
  import Symbolic.{ Atom, Sentence }
  import Sentential.symbolicSentences._, Ops._

  val oblivion = Knowledge.Oblivion[Symbolic.Sentence]
  import oblivion._

  import Knowledge.sat

  def atomsIn(s: Sentence): Set[Atom] = {
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

  type Assignment = Map[Atom, Boolean]

  def truthTable(s: Sentence)(atoms: Set[Atom] = atomsIn(s))
  : Map[Assignment, Boolean]
  = {
    require(atomsIn(s) subsetOf atoms)

    val allFalse = (atoms map (_ -> false) toMap)
    val assignments = 
      for (subset <- atoms.subsets) yield
        allFalse ++ (subset map (_ -> true))

    def evaluate(as: Assignment): Boolean = {
      def eval(s: Sentence): Boolean =
        s match {
          case False => false
          case True => true
          case p @ Atom(_) => as(p)
          case ¬(s) => !(eval(s))
          case And(p, q) => eval(p) && eval(q)
          case Or(p, q) => eval(p) || eval(q)
        }

      eval(s)
    }

    assignments.map(a => (a, evaluate(a))).toMap
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
    forAll { (s: Sentence) => atomsIn(reduce(s)) subsetOf atomsIn(s) }

  property("Reduction never alters the truth table") =
    forAll { (s: Sentence) => 
      val atoms = atomsIn(s)
      truthTable(s)(atoms) == truthTable(reduce(s))(atoms)
    }

  property("Conjunction with negation") =
    forAll { (s: Sentence) => reduce(s ∧ ¬(s)) == False }

  property("Disjunction with negation") =
    forAll { (s: Sentence) => reduce(s ∨ ¬(s)) == True }

  property("Conjunction with self") =
    forAll { (s: Sentence) =>
      truthTable(reduce(s ∧ s))(atomsIn(s)) == truthTable(s)(atomsIn(s))
    }

  property("Disjunction with self") =
    forAll { (s: Sentence) => 
      truthTable(reduce(s ∨ s))(atomsIn(s)) == truthTable(s)(atomsIn(s))
    }

  property("Satisfiability") =
    forAll { (s: Sentence) =>
      sat(s).forall { assignment =>
        val kb = assignment.foldLeft(Knowledge.Oblivion) { _ given _ }
        kb.reduce(s) == True
      }
    }

  // TODO: Rewrite this to construct implied sentences instead of hoping to
  //       receive them randomly
  // property("Implication is transitive") =
    forAll { (p: Sentence, q: Sentence, r: Sentence) => 
      ((reduce(p) != False && reduce(q) != False) &&
      given(p).reduce(q) == True &&
      given(q).reduce(r) == True) ==> (given(p).reduce(r) == True)
    }
} 
