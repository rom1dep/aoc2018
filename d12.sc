//val input: Iterator[String] =
//  """...## => #
//    |..#.. => #
//    |.#... => #
//    |.#.#. => #
//    |.#.## => #
//    |.##.. => #
//    |.#### => #
//    |#.#.# => #
//    |#.### => #
//    |##.#. => #
//    |##.## => #
//    |###.. => #
//    |###.# => #
//    |####. => #""".stripMargin.linesIterator
val input = scala.io.Source.fromFile("c12").getLines
//val initial = "#..#.#..##......###...###"
val initial = input.take(1).mkString.stripPrefix("initial state: ")

val rules: Map[String, String] = (for {
  line <- input if !line.isEmpty
  split = line.split(" => ")
} yield split(0) -> split(1)).toMap.withDefaultValue(".")

def gen(initState: String, initPad: Int = 0): (String, Int) = {
  val next = ("..." + initState + "...").sliding(5).map(rules(_)).mkString("")
  val padIndex = initPad + next.indexOf('#') - 1
  (next.dropWhile(_ == '.'), padIndex)
}

def cultivate(initial: String, iterations: Int = 20): (String, Int) =
  Iterator.iterate((initial, 0))((gen _).tupled).drop(iterations).next

def count(res: String, ixOffset: Int) = res.zipWithIndex.collect { case ('#', i) => i + ixOffset }.sum

// Solution for Part 1
(count _).tupled(cultivate(initial))

// Solution for Part 2
val targetIterations = 50000000000L
def cultivateStabilized(initial: String, iterations: BigInt = targetIterations) = {
  val memIterator = Iterator.iterate((0, initial, 0, "")) { oldGen =>
    val (currRes, currPad) = gen(oldGen._2, oldGen._3)
    (oldGen._1 + 1, currRes, currPad, oldGen._2)
  }
  val stabState = memIterator.dropWhile(gen => gen._2 != gen._4 && gen._1 < targetIterations).next
  (stabState._1, stabState._2, stabState._3) // GenId, Result string, Offset
}
val stableState = cultivateStabilized(initial)
val stableCount = count(stableState._2, stableState._3)
val stableNextCount = (count _).tupled(gen(stableState._2, stableState._3))

stableCount + (targetIterations - stableState._1) * (stableNextCount - stableCount)
