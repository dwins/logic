object main extends App {
  import dwins.logic._, dwins.logic.Symbolic._
  val system = implicitly[Sentential[Symbolic.Sentence]]
  import system.Ops._

  val kb = Knowledge.Oblivion.given('A)

  def test(p: Sentence) = println("%s ⇒ %s" format(p, kb.reduce(p)))

  println("Given " + kb)
  test('A)
  test('B)
  test(¬('A))
  test('A ∧ 'B)
  test('A ∨ 'B)
}
