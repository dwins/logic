object main extends App {
  import logic._, symbolic._

  val kb = Knowledge.Oblivion[Sentence].given('A)

  def test(p: Sentence) = println(kb.reduce(p))

  test('A)
  test('B)
  test(Not('A))
  test(And('A, 'B))
  test(Or('A, 'B))
}
