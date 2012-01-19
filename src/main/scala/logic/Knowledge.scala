package logic

sealed trait Knowledge[S] {
  def given(p: S): Knowledge[S]
  def satisfiabilityOf(p: S): Satisfiability
  def reduce(p: S): S
}

object Knowledge {
  def Oblivion[S : Sentential]: Knowledge[S] = new Facts[S](Set.empty)

  def Absurdity[S]: Knowledge[S] = absurdity.asInstanceOf[Knowledge[S]]

  private val absurdity: Knowledge[Any] = new Knowledge[Any] {
    def given(p: Any) =
      sys.error("Tried to add givens to an already inconsistent set")

    def satisfiabilityOf(p: Any) =
      sys.error("Tried to determine satisfiability with inconsistent givens")

    override def reduce(p: Any) =
      sys.error("Tried to reduce with inconsistent givens")

    override def toString = "Absurdity"
  }

  private class Alternatives[S : Sentential](worlds: Seq[Knowledge[S]]) extends Impl[S] {
    require(worlds forall(Absurdity !=),
      "Alternatives should not be created with Absurdity as a possible world")

    def given(p: S): Knowledge[S] =
      possibleWorlds(worlds map (_ given p))

    def satisfiabilityOf(p: S): Satisfiability =
      worlds.map(_ satisfiabilityOf p).distinct match {
        case Seq(Always) => Always
        case Seq(Never) => Never
        case _ => Sometimes
      }

    override def toString = worlds mkString(" || ")
  }

  private class Facts[S](facts: Set[S])(implicit system: Sentential[S]) extends Impl[S] {
    import system._, Ops._
    require(facts forall isLiteral,
      "Knowledge should only be in terms of literals (Atom or ¬(Atom))")

    def given(p: S): Knowledge[S] = {
      p match {
        case p @ Literal() =>
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

    def satisfiabilityOf(p: S) = 
      p match {
        case True => Always
        case False => Never
        case p @ Literal() =>
          if (provenBy(facts, p))
            Always
          else if (disprovenBy(facts, p))
            Never
          else
            Sometimes
        case _ => Sometimes
      }

    override def toString = facts.mkString("[", ", ", "]")
  }

  private abstract class Impl[S : Sentential] extends Knowledge[S] {
    def reduce(p: S): S = {
      @annotation.tailrec
      def iterate(p: S, limit: Int): S =
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

  private def possibleWorlds[S : Sentential](ws: Seq[Knowledge[S]]): Knowledge[S] =
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
  private def simplifyOnce[S](p: S, kb: Knowledge[S])(implicit system: Sentential[S]): S = {
    import system._, Ops._
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


