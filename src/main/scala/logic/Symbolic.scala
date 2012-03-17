package logic

package object symbolic {
  implicit def symbolAsAtom(s: Symbol): Sentence = Atom(s)
}

package symbolic {
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
}
