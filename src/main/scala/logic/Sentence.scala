package logic

trait Sentential[Sentence] {
  val False: Sentence
  val True: Sentence

  def not(p: Sentence): Sentence
  def extractNot(p: Sentence): Option[Sentence]

  def and(p: Sentence, q: Sentence): Sentence
  def extractAnd(p: Sentence): Option[(Sentence, Sentence)]

  def or(p: Sentence, q: Sentence): Sentence
  def extractOr(p: Sentence): Option[(Sentence, Sentence)]

  def isLiteral(p: Sentence): Boolean
  def provenBy(givens: Set[Sentence], s: Sentence): Boolean
  def disprovenBy(givens: Set[Sentence], s: Sentence): Boolean

  object Ops {
    object Not {
      def apply(p: Sentence): Sentence = not(p)
      def unapply(p: Sentence): Option[Sentence] = extractNot(p)
    }

    object And {
      def apply(p: Sentence, q: Sentence): Sentence = and(p, q)
      def unapply(p: Sentence): Option[(Sentence, Sentence)] = extractAnd(p)
    }

    object Or {
      def apply(p: Sentence, q: Sentence): Sentence = or(p, q)
      def unapply(p: Sentence): Option[(Sentence, Sentence)] = extractOr(p)
    }

    object Literal {
      def unapply(p: Sentence): Boolean = isLiteral(p)
    }

    val ¬ = Not
    val ∧ = And
    val ∨ = Or

    class InfixSentenceOperators(p: Sentence) {
      def ∧(q: Sentence): Sentence = and(p, q)
      def ∨(q: Sentence): Sentence = or(p, q)
    }

    implicit def enrichSentence(p: Sentence) = new InfixSentenceOperators(p)
    implicit def enrichSentenceView[P <% Sentence](p: P) = new InfixSentenceOperators(p)
  }
}

object Sentential {
  implicit object symbolicSentences extends Sentential[Symbolic.Sentence] {
    import Symbolic._
    val True = Symbolic.True
    val False = Symbolic.False

    def and(p: Sentence, q: Sentence): Sentence = And(p, q)
    def extractAnd(p: Sentence): Option[(Sentence, Sentence)] =
      p match {
        case And(p, q) => Some((p, q))
        case _ => None
      }

    def or(p: Sentence, q: Sentence): Sentence = Or(p, q)
    def extractOr(p: Sentence): Option[(Sentence, Sentence)] =
      p match {
        case Or(p, q) => Some((p, q))
        case _ => None
      }

    def not(p: Sentence): Sentence = Not(p)
    def extractNot(p: Sentence): Option[Sentence] =
      p match {
        case Not(p) => Some(p)
        case _ => None
      }

    def isLiteral(p: Sentence): Boolean =
      p match {
        case Atom(_) | Not(Atom(_)) => true
        case _ => false
      }

    def provenBy(facts: Set[Sentence], s: Sentence): Boolean =
      facts contains s

    def disprovenBy(facts: Set[Sentence], s: Sentence): Boolean = {
      import Ops._
      s match {
        case p @ Atom(_) => facts contains ¬(p)
        case ¬(Atom(p)) => facts contains p
        case _ => sys.error("Tried to test a non-literal against a knowledge base")
      }
    }
  }
}
