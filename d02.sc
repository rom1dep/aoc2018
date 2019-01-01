val input: List[String] = scala.io.Source.fromFile("c2").getLines.toList

//puzzle 1
val res = input.map(_.groupBy(identity).mapValues(_.length)).foldLeft(0, 0) {
  case ((doubles, triples), m: Map[Char, Int]) => (
    if (m.values.contains(2)) doubles + 1 else doubles,
    if (m.values.contains(3)) triples + 1 else triples,
  )
}
println(res._1 * res._2)

//puzzle 2
val good_ids = input.filter { s =>
  val m = s.groupBy(identity).mapValues(_.length)
  m.values.contains(2) || m.values.contains(3)
}

good_ids.combinations(2).find {
  case s1 :: s2 :: Nil => (s1 zip s2).foldLeft(0) {
    case (cnt, (c1, c2)) =>
      if (c1 != c2) cnt + 1 else cnt
  } == 1
}.get
