object DayTwentyTwo extends App {
  // Sample Puzzle
  // val depth = 510
  // val (xt, yt) = (10, 10) //target
  val depth = 11820
  val (xt, yt) = (7, 782)

  //This stores the map of the cave, we need caching so we buy the convenience of a mutable map
  import scala.collection.mutable
  val m = mutable.Map.empty[(Int, Int), BigInt]

  def geoIndex(xa: Int, ya: Int): BigInt =
    // If the region's Y coordinate is 0, the geologic index is its X coordinate times 16807.
    // If the region's X coordinate is 0, the geologic index is its Y coordinate times 48271.
    // Otherwise, the region's geologic index is the result of multiplying the erosion levels of the regions at X-1,Y and X,Y-1.
    m.getOrElseUpdate(
      (xa, ya),
      (xa, ya) match {
        case (0, 0)       => 0
        case (x, 0)       => x * 16807
        case (0, y)       => y * 48271
        case (`xt`, `yt`) => 0
        case (x, y)       => erosLevel(x - 1, y) * erosLevel(x, y - 1)
      }
    )

  //A region's erosion level is its geologic index plus the cave system's depth, all modulo 20183.
  def erosLevel(xa: Int, ya: Int): BigInt = (geoIndex(xa, ya) + depth) % 20183

  def regVal(xa: Int, ya: Int): Int = erosLevel(xa, ya).toInt % 3

  def regType(xa: Int, ya: Int): Char = regVal(xa, ya) match {
    //If the erosion level modulo 3 is 0, the region's type is rocky.
    //If the erosion level modulo 3 is 1, the region's type is wet.
    //If the erosion level modulo 3 is 2, the region's type is narrow.
    //rocky as ., wet as =, narrow as |, the mouth as M, the target as T
    case 0 => '.' //'R'
    case 1 => '=' //'W'
    case 2 => '|' //'N'
  }

  def printCave(): Unit = {
    for { y <- 0 to 16; x <- 0 to 16 } {
      if (x == 16) print('\n')
      else if (x == 0 && y == 0) print('M')
      else if (x == xt && y == yt) print('T')
      else print(regType(x, y))
    }
  }

  //Solution to Part 1:
  println((for { y <- 0 to yt; x <- 0 to xt } yield regVal(x, y)).sum)

  //Part 2, we need a path finder!
  //Let's have a proper map of the cave, embiggened for optimal path search
  val (maxX, maxY) = (xt + 30, yt + 30)
  type Terrain = Int
  val map: Map[(Int, Int), Terrain] =
    (for { y <- 0 to maxY; x <- 0 to maxX } yield
      ((x, y) -> regVal(x, y))).toMap

  sealed trait Direction
  case object Left extends Direction
  case object Right extends Direction
  case object Up extends Direction
  case object Down extends Direction

  sealed trait Tool
  case object Climbing extends Tool
  case object Torch extends Tool
  case object Neither extends Tool

  @inline case class Pos(x: Int, y: Int)

  @inline case class Move(p: Pos, te: Terrain, tl: Tool) {
    def step(d: Direction): Move = {
      val newPos = d match {
        case Left  => p.copy(x = p.x - 1)
        case Right => p.copy(x = p.x + 1)
        case Up    => p.copy(y = p.y - 1)
        case Down  => p.copy(y = p.y + 1)
      }
      copy(p = newPos, te = map.getOrElse((newPos.x, newPos.y), -1))
    }

    def swap(t: Tool): Move = copy(tl = t)

    def isLegit(): Boolean =  p.x >= 0 && p.y >= 0 && Move.allowedTools(te).contains(tl)

    private def nextMoves = List((step(Left), 1),
                                 (step(Right), 1),
                                 (step(Up), 1),
                                 (step(Down), 1),
                                 (swap(Climbing), 7),
                                 (swap(Torch), 7),
                                 (swap(Neither), 7))

    def nextLegitMoves = nextMoves.filter(_._1.isLegit)
  }

  @inline object Move {
    def allowedTools(te: Terrain) = List(Neither, Torch, Climbing).filter { tl =>
      //Tools can only be used in certain regions:
      //  In rocky regions, you can use the climbing gear or the torch. You cannot use neither (you'll likely slip and fall).
      //  In wet regions, you can use the climbing gear or neither tool. You cannot use the torch (if it gets wet, you won't have a light source).
      //  In narrow regions, you can use the torch or neither tool. You cannot use the climbing gear (it's too bulky to fit).
      te match {
        case 0 => tl != Neither
        case 1 => tl != Torch
        case 2 => tl != Climbing
        case _ => false
      }
    }
  }

  object PathFinder {
    import scalax.collection.Graph
    import scalax.collection.edge.WDiEdge
    import scalax.collection.edge.Implicits._

    val edges = for {
      x <- 0 to maxX; y <- 0 to maxY;
      terrain = map((x, y))
      tool <- Move.allowedTools(terrain)
      current = Move(Pos(x, y), terrain, tool)
      (next, weight) <- current.nextLegitMoves
    } yield (current ~%> next)(weight)

    val graph = Graph.from(None, edges.toList)

    val (startMove, targetMove) = (Move(Pos(0, 0), 0, Torch), Move(Pos(xt, yt), 0, Torch))

    lazy val solution = graph.get(startMove) shortestPathTo graph.get(targetMove)
  }

  //Solution to Part 2:
  println(PathFinder.solution.get.weight)
}
