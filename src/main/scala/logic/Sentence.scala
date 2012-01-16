package logic

object Sentence {
  sealed trait Sentence
  case object False extends Sentence
  case object True extends Sentence
  case class Atom(s: Symbol) extends Sentence {
    override def toString = s.toString
  }
  case class Not(p: Sentence) extends Sentence {
    override def toString = "¬" + p.toString
  }
  case class And(p: Sentence, q: Sentence) extends Sentence {
    override def toString = "(%s ∧ %s)" format(p, q)
  }
  case class Or(p: Sentence, q: Sentence) extends Sentence {
    override def toString = "(%s ∨ %s)" format(p, q)
  }

  class Ops(p: Sentence) {
    def ∧(q: Sentence) = And(p, q)
    def ∨(q: Sentence) = Or(p, q)
  }

  val ¬ = Not
  val ∧ = And
  val ∨ = Or

  implicit def symbolAsAtom(s: Symbol) = Atom(s)

  implicit def ops[S <% Sentence](p: S) = new Ops(p)

  def isLiteral(p: Sentence) = 
    p match {
      case Atom(_) | ¬(Atom(_)) => true
      case _ => false
    }

  def provenBy(facts: Set[Sentence], s: Sentence): Boolean =
    facts contains s

  def disprovenBy(facts: Set[Sentence], s: Sentence): Boolean =
    s match {
      case p @ Atom(_) => facts contains ¬(p)
      case ¬(Atom(p)) => facts contains p
      case _ => sys.error("Tried to test a non-literal against a knowledge base")
    }
}
