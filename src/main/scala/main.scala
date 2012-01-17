object main extends App {
  import logic._, Symbolic._
  val system = implicitly[Sentential[Symbolic.Sentence]]
  import system.Ops._

  val kb = Knowledge.Oblivion.given('A)

  def test(p: Sentence) = println(kb.reduce(p))

  test('A)
  test('B)
  test(¬('A))
  test('A ∧ 'B)
  test('A ∨ 'B)
}
