import logic._, Sentence._
object main extends App {
  class Schmowledge(truth: Seq[Sentence]) {
    def given(p: Sentence) = new Schmowledge(truth :+ p)

    def satisfiable(p: Sentence) = simplify(p) != False

    def satisfiabilityOf(p: Sentence): Satisfiability =
      p match {
        case p @ Atom(_) =>
          if (truth contains p)
            Always
          else if (truth contains ¬(p))
            Never
          else Sometimes
        case p @ ¬(n @ Atom(_)) =>
          if (truth contains p)
            Always
          else if (truth contains n)
            Never
          else Sometimes
        case _ => Sometimes
      }

    def simplify(p: Sentence): Sentence = {
      def helper(p: Sentence): Sentence =
        p match {
          case p if satisfiabilityOf(p) == Always => True
          case p if satisfiabilityOf(p) == Never => False
          case ¬(¬(p)) => p
          case ¬(p) => ¬(simplify(p))
          case p @ (False | True | Atom(_)) => p
          case orig @ (p And q) =>
            lazy val q_ = given(p).simplify(q)
            lazy val p_ = given(q).simplify(p)
            if (p_ == False || q_ == False)
              False
            if (q_ == True)
              p
            else if (p_ == True)
              q
            else if (p_ != p)
              p_ ∧ q
            else if (q_ != q)
              p ∧ q_
            else orig
          case orig @ (p Or q) =>
            lazy val q_ = given(¬(p)).simplify(q)
            lazy val p_ = given(¬(q)).simplify(p)
            if (p_ == True || q_ == True)
              True
            else if (p_ == False)
              q
            else if (q_ == False)
              p
            else if (p_ != p)
              p_ ∨ q
            else if (q_ != q)
              p ∨ q_
            else orig
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

  val kb = Knowledge('A)

  def test(p: Sentence) = println(kb.reduce(p))

  test('A)
  test('B)
  test(¬('A))
  test('A ∧ 'B)
  test('A ∨ 'B)
}
