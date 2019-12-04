package day3

import scala.io.Source

object Solution extends App {


  type Coord = (Int, Int)
  type Segment = (Coord, Coord)
  //  val input = Source.fromResource("day3/input.txt").getLines().foreach(println)

  //  val input = """R75,D30,R83,U83,L12,D49,R71,U7,L72
  //                |U62,R66,U55,R34,D71,R55,D58,R83""".stripMargin

  val input =
    """R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
      |U98,R91,D20,R16,D67,R40,U7,R15,U6,R7""".stripMargin
  //  val input = """R8,U5,L5,D3
  //                |U7,R6,D4,L4""".stripMargin

  val test = Source.fromResource("day3/input.txt").getLines().map(_.split(',').foldLeft(((0, 0), List.empty[Segment])) { case (acc: (Coord, List[Segment]), instruction: String) =>
    //  val test = input.linesIterator.map(_.split(',').foldLeft(((0,0), List.empty[Segment])) { case (acc: (Coord, List[Segment]), instruction: String) =>
    val pattern = "([UDLR])([0-9]+)".r
    val pattern(direction, count) = instruction

    val beginPoint = acc._1
    val endPoint = direction match {
      case "U" => (beginPoint._1, beginPoint._2 + count.toInt)
      case "D" => (beginPoint._1, beginPoint._2 - count.toInt)
      case "L" => (beginPoint._1 - count.toInt, beginPoint._2)
      case "R" => (beginPoint._1 + count.toInt, beginPoint._2)
    }
    val segment: (Coord, Coord) = (beginPoint, endPoint)
    (endPoint, segment :: acc._2)

  })


  //  println(test.toList)

  val rr = test.toList match {
    case wire1 :: wire2 :: Nil =>
      for {
        segment1 <- wire1._2
        segment2 <- wire2._2
      } yield intersection(segment1, segment2).map { isc => (isc, distanceTo(isc, wire1._2.reverse), distanceTo(isc, wire2._2.reverse)) }

  }

  //  println(rr.flatten.filter { _ != (0,0)}.map { it => Math.abs(it._1) + Math.abs(it._2) }.sorted.headOption)
  println(rr.flatten.filter {
    _._1 != (0, 0)
  }.map { case (a, b, c) => b + c }.sorted.headOption)


  def distanceTo(intersection: Coord, wire: List[Segment], totalTraveled: Int = 0): Int = {

    def isOnSegment(coord: Coord, segment: Segment): Option[Int] = {
      if (segment._1._1 == segment._2._1) {
        //vertical wire
        if (coord._1 == segment._1._1 && coord._2 <= Math.max(segment._1._2, segment._2._2) && coord._2 >= Math.min(segment._1._2, segment._2._2)) {
          Some(Math.abs(coord._2 - segment._1._2))
        } else {
          None
        }
      }

      else if (segment._1._2 == segment._2._2) {
        //horizontal wire
        if (coord._2 == segment._1._2 && coord._1 <= Math.max(segment._1._1, segment._2._1) && coord._1 >= Math.min(segment._1._1, segment._2._1)) {
          Some(Math.abs(coord._1 - segment._1._1))
        } else {
          None
        }
      } else {
        None
      }

    }

    wire match {
      case head :: tail => isOnSegment(intersection, head) match {
        case Some(distance) =>
          totalTraveled + distance
        case None =>
          val length = Math.sqrt(Math.pow(head._1._1 - head._2._1, 2) + Math.pow(head._1._2 - head._2._2, 2)).toInt
          distanceTo(intersection, tail, totalTraveled + length)
      }
    }
  }

  def intersection(segment1: Segment, segment2: Segment): Option[Coord] = {
    val p1 = segment1._1
    val q1 = segment1._2

    val p2 = segment2._1
    val q2 = segment2._2


    //segment1 is y-axis-aligned, segment2 is x-axis-aligned
    if (p1._1 - q1._1 == 0 && p2._2 - q2._2 == 0 &&
      Math.max(p1._2, q1._2) >= Math.max(p2._2, q2._2) && Math.min(p1._2, q1._2) <= Math.min(p2._2, q2._2) &&
      Math.max(p2._1, q2._1) >= Math.max(p1._1, q1._1) && Math.min(p2._1, q2._1) <= Math.min(p1._1, q1._1))

      Some((p1._1, p2._2))

    //segment1 is x-axis-aligned, segment2 is y-axis-aligned
    else if (p1._2 - q1._2 == 0 && p2._1 - q2._1 == 0 &&
      Math.max(p1._1, q1._1) >= Math.max(p2._1, q2._1) && Math.min(p1._1, q1._1) <= Math.min(p2._1, q2._1) &&
      Math.max(p2._2, q2._2) >= Math.max(p1._2, q1._2) && Math.min(p2._2, q2._2) <= Math.min(p1._2, q1._2))

      Some((p2._1, p1._2))

    else None
  }

  val s1 = ((4, 3), (9, 3))
  val s2 = ((7, 1), (7, 5))

  //  println(intersection(s1,s2))
  //  println(intersection(s2,s1))

}


