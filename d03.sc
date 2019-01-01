val input: List[String] = scala.io.Source.fromFile("c3").getLines.toList
val pattern = raw"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)".r

//Very unidiomatic, non-FP solution

//puzzle 1
val fabric = Array.fill(1000, 1000)(collection.mutable.Set.empty[Int])
var ids = collection.mutable.Set.empty[Int]

input.foreach {
  case pattern(_id, _px, _py, _sx, _sy) =>
    val (id, px, py, sx, sy) = (_id.toInt, _px.toInt, _py.toInt, _sx.toInt, _sy.toInt)
    ids += id
    for {x <- px until px + sx; y <- py until py + sy} {
      fabric(y)(x) += id
    }
}

fabric.foldLeft(0) {
  case (acc, l) => acc + l.count(_.size >= 2)
}

//puzzle 2
for {x <- fabric.indices; y <- fabric(x).indices
     if fabric(y)(x).size > 1
     e <- fabric(y)(x)} {
  ids -= e
}
ids
