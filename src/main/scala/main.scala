object main extends App {
  import logic._
  import symbolic.{ Sentence, symbolAsAtom }
  val system = implicitly[Sentential[symbolic.Sentence]]
  import system.Ops._

  val kb = Knowledge.Oblivion.given('A)

  def test(p: Sentence) = println(kb.reduce(p))

  test('A)
  test('B)
  test(Not('A))
  test(And('A, 'B))
  test(Or('A, 'B))
}
