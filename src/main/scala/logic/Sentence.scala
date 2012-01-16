package logic

trait Sentential {
  type Sentence
  
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

  sealed trait Knowledge {
    import Ops._

    def given(p: Sentence): Knowledge
    def satisfiabilityOf(p: Sentence): Satisfiability
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

  val Oblivion: Knowledge = new Facts(Set.empty)

  val Absurdity: Knowledge = new Knowledge {
    def given(p: Sentence) =
      sys.error("Tried to add givens to an already inconsistent set")

    def satisfiabilityOf(p: Sentence) =
      sys.error("Tried to determine satisfiability with inconsistent givens")

    override def reduce(p: Sentence) =
      sys.error("Tried to reduce with inconsistent givens")

    override def toString = "Absurdity"
  }

  private class Alternatives(worlds: Seq[Knowledge]) extends Knowledge {
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

  private class Facts(facts: Set[Sentence]) extends Knowledge {
    require(facts forall isLiteral,
      "Knowledge should only be in terms of literals (Atom or ¬(Atom))")

    import Ops._

    def given(p: Sentence): Knowledge = {
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

    def satisfiabilityOf(p: Sentence) = 
      p match {
        case True => Always
        case False => Never
        case p @ Literal() =>
          if (provenBy(facts, p))
            Always
          else if (disprovenBy(facts, p))
            Never
          else Sometimes
        case _ => Sometimes
      }

    override def toString = facts.mkString("[", ", ", "]")
  }

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
    import Ops._
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

object Sentence extends Sentential {
  sealed trait Sentence
  case object False extends Sentence
  case object True extends Sentence

  private case class Atom(s: Symbol) extends Sentence {
    override def toString = s.toString
  }

  private case class Not(p: Sentence) extends Sentence {
    override def toString = "¬" + p.toString
  }

  private case class And(p: Sentence, q: Sentence) extends Sentence {
    override def toString = "(%s ∧ %s)" format(p, q)
  }

  private case class Or(p: Sentence, q: Sentence) extends Sentence {
    override def toString = "(%s ∨ %s)" format(p, q)
  }

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

  implicit def symbolAsAtom(s: Symbol): Sentence = Atom(s)

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

object Knowledge {
  val logicSystem = (Sentence: Sentential)
  import logicSystem._, Ops._

  def apply(facts: Sentence*) = (facts foldLeft Oblivion)(_ given _)
}
