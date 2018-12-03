val input: List[Int] = scala.io.Source.fromFile("c1").getLines.map(_.toInt).toList

//puzzle 1
input.sum

//puzzle 2
Iterator.continually(input).flatten.scanLeft((0, Set.empty[Int])) {
  case ((sum, visited), current) => (sum + current, visited + sum)
}.find { case (sum, visited) => visited.contains(sum) }.get
