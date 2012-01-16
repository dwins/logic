import logic.Sentence._, Ops._
object main extends App {
  val kb = Oblivion.given('A)

  def test(p: Sentence) = println(kb.reduce(p))

  test('A)
  test('B)
  test(¬('A))
  test('A ∧ 'B)
  test('A ∨ 'B)
}
