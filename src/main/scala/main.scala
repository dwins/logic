import logic.Sentential, logic.Symbolic._
object main extends App {
  val system = implicitly[Sentential[Sentence]]
  import system.{ Absurdity, Knowledge, Oblivion }, system.Ops._

  val kb = Oblivion.given(Atom('A))

  def test(p: Sentence) = println(kb.reduce(p))

  test('A)
  test('B)
  test(¬('A))
  test('A ∧ 'B)
  test('A ∨ 'B)
}
