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

object Knowledge {
  trait Knowledge {
    def given(p: Sentence): Knowledge
    def satisfiabilityOf(p: Sentence): Satisfiability
    def reduce(p: Sentence): Sentence
  }

  private abstract class Impl extends Knowledge {
    def reduce(p: Sentence): Sentence = {
      @annotation.tailrec
      def iterate(p: Sentence, limit: Int): Sentence =
        if (limit <= 0) {
          p
        } else {
          val p_ = simplifyOnce(p, this)
          if (p == p_)
            p
          else
            iterate(p_, limit - 1)
        }

      iterate(p, 10)
    }
  }

  private class Facts(facts: Set[Sentence]) extends Impl {
    require(facts forall(Sentence.isLiteral),
      "Knowledge should only be in terms of literals (Atom or ¬(Atom))")

    def given(p: Sentence): Knowledge = {
      p match {
        case p @ (Atom(_) | ¬(Atom(_))) =>
          if (disprovenBy(facts, p))
            Absurdity
          else
            new Facts(facts + p)
        case ¬(¬(p)) =>
          given(p)
        case ¬(Or(p, q)) =>
          val pFalse = given(¬(p))
          if (pFalse == Absurdity) Absurdity else pFalse.given(¬(q))
        case And(p, q) =>
          val pTrue = given(p)
          if (pTrue == Absurdity) Absurdity else pTrue.given(q)
        case Or(p, q) =>
          possibleWorlds(Seq(given(p), given(q)))
        case ¬(And(p, q)) =>
          possibleWorlds(Seq(given(¬(p)), given(¬(q))))
        case True | ¬(False) =>
          this
        case False | ¬(True) =>
          Absurdity
      }
    }

    def satisfiabilityOf(p: Sentence) = 
      p match {
        case True => Always
        case False => Never
        case p @ (Atom(_) | ¬(Atom(_))) =>
          if (provenBy(facts, p))
            Always
          else if (disprovenBy(facts, p))
            Never
          else Sometimes
        case _ => Sometimes
      }

    override def toString = facts.mkString("[", ", ", "]")
  }

  private class Alternatives(worlds: Seq[Knowledge]) extends Impl {
    require(worlds forall(Absurdity !=),
      "Alternatives should not be created with Absurdity as a possible world")

    def given(p: Sentence): Knowledge = {
      possibleWorlds(worlds map (_ given p))
    }

    def satisfiabilityOf(p: Sentence): Satisfiability =
      worlds.map(_ satisfiabilityOf p).distinct match {
        case Seq(Always) => Always
        case Seq(Never) => Never
        case _ => Sometimes
      }

    override def toString = worlds mkString(" || ")
  }

  val Oblivion: Knowledge = new Facts(Set.empty)

  val Absurdity: Knowledge = new Knowledge {
    def given(p: Sentence) =
      sys.error("Tried to add givens to an already inconsistent set")

    def satisfiabilityOf(p: Sentence) =
      sys.error("Tried to determine satisfiability with inconsistent givens")

    def reduce(p: Sentence) =
      sys.error("Tried to reduce with inconsistent givens")

    override def toString = "Absurdity"
  }

  def apply(facts: Sentence*) = (facts foldLeft Oblivion)(_ given _)

  private def possibleWorlds(ws: Seq[Knowledge]): Knowledge =
    (ws filter(Absurdity !=)) match {
      case Seq() => Absurdity
      case Seq(w) => w
      case ws => new Alternatives(ws)
    }

  /**
   * Use a Knowledge base to simplify a Sentence. If no applicable reductions
   * are found, the original sentence will be returned.  This method only
   * simplifies one "level"; it may be effective to call it repetitively to get
   * multiple levels of reduction.
   */
  private def simplifyOnce(p: Sentence, kb: Knowledge): Sentence = {
    p match {
      case p @ (False | True) => p
      case ¬(True) => False
      case ¬(False) => True
      case ¬(¬(p)) => p
      case ¬(p) => ¬(simplifyOnce(p, kb))
      case orig @ (p And q) =>
        val pTrue = kb.given(p)
        val qTrue = kb.given(q)
        if (pTrue == Absurdity || qTrue == Absurdity)
          False
        else {
          val q_ = simplifyOnce(q, pTrue)
          val p_ = simplifyOnce(p, qTrue)
          if (p_ == False || q_ == False) False
          else if (q_ == True)            p
          else if (p_ == True)            q
          else if (p_ != p)               p_ ∧ q
          else if (q_ != q)               p ∧ q_
          else                            orig
        }
      case orig @ (p Or q) =>
        val pFalse = kb.given(¬(p))
        val qFalse = kb.given(¬(q))
        if (pFalse == Absurdity || qFalse == Absurdity)
          True
        else {
          val q_ = simplifyOnce(q, pFalse)
          val p_ = simplifyOnce(p, qFalse)
          if (p_ == True || q_ == True) True
          else if (q_ == False)         p
          else if (p_ == False)         q
          else if (p_ != p)             p_ ∨ q
          else if (q_ != q)             p ∨ q_
          else                          orig
        }
      case p => 
        val sat = kb.satisfiabilityOf(p)
        if (sat == Always)     True
        else if (sat == Never) False
        else                   p
    }
  }
}
}
