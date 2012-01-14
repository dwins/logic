package logic
trait Knowledge {
  def given(p: Sentence): Knowledge
  def satisfiabilityOf(p: Sentence): Satisfiability
  def reduce(p: Sentence): Sentence
}

object Knowledge {
  private abstract class Impl extends Knowledge {
    def reduce(p: Sentence): Sentence = {
      def helper(p: Sentence): Sentence =
        p match {
          case p @ (False | True) => p
          case ¬(True) => False
          case ¬(False) => True
          case ¬(¬(p)) => p
          case ¬(p) => ¬(helper(p))
          case orig @ (p And q) =>
            val pTrue = given(p)
            val qTrue = given(q)
            if (pTrue == Absurdity || qTrue == Absurdity)
              False
            else {
              val q_ = pTrue.reduce(q)
              val p_ = qTrue.reduce(p)
              if (p_ == False || q_ == False) False
              else if (q_ == True)            p
              else if (p_ == True)            q
              else if (p_ != p)               p_ ∧ q_
              else if (q_ != q)               p ∧ q_
              else                            orig
            }
          case orig @ (p Or q) =>
            val pFalse = given(¬(p))
            val qFalse = given(¬(q))
            if (pFalse == Absurdity || qFalse == Absurdity)
              True
            else {
              val q_ = pFalse.reduce(q)
              val p_ = qFalse.reduce(p)
              if (p_ == True || q_ == True) True
              else if (q_ == False)         p
              else if (p_ == False)         q
              else if (p_ != p)             p_ ∨ q_
              else if (q_ != q)             p ∨ q_
              else                          orig
            }
          case p => 
            val sat = satisfiabilityOf(p)
            if (sat == Always)     True
            else if (sat == Never) False
            else                   p
        }

      @annotation.tailrec
      def iterate(p: Sentence, limit: Int): Sentence =
        if (limit <= 0) {
          p
        } else {
          val p_ = helper(p)
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
        case p @ Atom(_) =>
          if (impliedBy(facts, ¬(p)))
            Absurdity
          else
            new Facts(facts + p)
        case p @ ¬(Atom(n)) =>
          if (impliedBy(facts, n))
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
        case p @ Atom(_) =>
          if (impliedBy(facts, p))
            Always
          else if (impliedBy(facts, ¬(p)))
            Never
          else Sometimes
        case p @ ¬(n @ Atom(_)) =>
          if (impliedBy(facts, p))
            Always
          else if (impliedBy(facts, n))
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
      (worlds foldLeft (Never: Satisfiability)) { (s, k) =>
        (s, k satisfiabilityOf p) match {
          case (Always, _) | (_, Always) => Always
          case (Sometimes, _) | (_, Sometimes) => Sometimes
          case _ => Never
        }
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

  private def impliedBy(facts: Set[Sentence], s: Sentence): Boolean =
    facts contains s
}
