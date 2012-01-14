package logic

sealed trait Sentence
case object False extends Sentence
case object True extends Sentence
case class Atom(s: Symbol) extends Sentence {
  override def toString = s.toString
}
case class ¬(p: Sentence) extends Sentence {
  override def toString = "¬" + p.toString
}
case class And(p: Sentence, q: Sentence) extends Sentence {
  override def toString = "(%s ∧ %s)" format(p, q)
}
case class Or(p: Sentence, q: Sentence) extends Sentence {
  override def toString = "(%s ∨ %s)" format(p, q)
}

object Sentence {
  class Ops(p: Sentence) {
    def ∧(q: Sentence) = And(p, q)
    def ∨(q: Sentence) = Or(p, q)
  }

  implicit def symbolAsAtom(s: Symbol) = Atom(s)

  implicit def ops[S <% Sentence](p: S) = new Ops(p)

  def isLiteral(p: Sentence) = 
    p match {
      case Atom(_) | ¬(Atom(_)) => true
      case _ => false
    }
}
