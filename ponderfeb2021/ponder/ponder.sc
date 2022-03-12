import java.io._
object ponder {
  val allPeople = (for {
    i <- 0 until 12
    j <- 0 until 12
  } yield (i, j)).toSet                           //> allPeople  : scala.collection.immutable.Set[(Int, Int)] = Set((7,1), (7,5), 
                                                  //| (1,8), (7,6), (2,5), (10,3), (3,11), (1,5), (7,9), (9,10), (5,0), (8,10), (1
                                                  //| 0,5), (11,7), (11,6), (9,0), (6,7), (8,9), (3,9), (0,2), (0,0), (11,5), (5,2
                                                  //| ), (3,10), (6,10), (11,1), (7,4), (5,1), (4,0), (4,10), (3,4), (6,4), (0,10)
                                                  //| , (7,7), (4,7), (2,10), (0,9), (6,6), (5,11), (7,8), (3,1), (9,1), (6,1), (4
                                                  //| ,1), (5,9), (11,4), (6,2), (8,1), (0,7), (2,0), (0,3), (6,9), (4,4), (3,0), 
                                                  //| (8,11), (8,0), (5,10), (10,10), (1,6), (1,11), (0,5), (10,1), (3,6), (6,5), 
                                                  //| (2,8), (6,8), (11,10), (11,3), (1,1), (6,3), (6,11), (3,5), (7,3), (1,10), (
                                                  //| 4,6), (8,3), (4,11), (10,7), (9,8), (1,9), (11,8), (4,5), (1,4), (2,6), (10,
                                                  //| 2), (8,2), (0,8), (11,2), (8,8), (4,9), (0,4), (2,9), (5,7), (5,4), (9,7), (
                                                  //| 7,11), (3,2), (10,6), (7,10), (11,9), (1,3), (11,11), (2,2), (5,5), (4,8), (
                                                  //| 10,11), (2,7), (4,2), (2,4), (3,7), (0,11), (10,9), (0,1), (10,4), (9,6), (8
                                                  //| ,4), (5,3), (2,11), (5,8
                                                  //| Output exceeds cutoff limit.

  def printUnvaccinated(unvaccinated: Set[(Int, Int)]) =
    (0 until 12).foreach { i =>
      println((0 until 12).map(j => if (unvaccinated.contains((i, j))) 'X' else '.').mkString)
    }                                             //> printUnvaccinated: (unvaccinated: Set[(Int, Int)])Unit
 
  val connectionsAscii = List(
    "0a8301b11b01",
    "1bda41b24d78",
    "37c09e8d5998",
    "60473283d3b8",
    "13279043d9bc",
    "371bf4c021c1",
    "1d122e800ee1",
    "5bc967265d88",
    "5f1998f5915d",
    "628dff094034",
    "39effbe6ecc8",
    "2c440c20e0a0")                               //> connectionsAscii  : List[String] = List(0a8301b11b01, 1bda41b24d78, 37c09e8d
                                                  //| 5998, 60473283d3b8, 13279043d9bc, 371bf4c021c1, 1d122e800ee1, 5bc967265d88, 
                                                  //| 5f1998f5915d, 628dff094034, 39effbe6ecc8, 2c440c20e0a0)
     
   /*
  val connectionsAscii = List(
    "414141414141", // 6
    "414141414141", // 12
    "414141414141", // 18
    "414141414141", // 24
    "414141414141", // 30
    "414141414141", // 36
    "414141414141", // 42
    "000000000000", // 8
    "000000000000", // 8
    "000000000000", // 8
    "000000000000", // 8
    "000000000000") // 12
     */
     
      
    
  val originalSolution = Set((4,8), (9,1), (5,10), (1,1), (1,10), (2,8), (9,10), (4,3), (1,7), (0,3), (3,10), (10,8), (10,5), (7,9), (7,5), (6,1), (6,10), (4,4), (1,4), (4,6), (9,4), (1,9), (8,1), (1,6), (7,7), (3,9), (7,1), (10,3), (3,3), (8,7), (5,1), (8,10), (7,3), (7,4), (7,6))
                                                  //> originalSolution  : scala.collection.immutable.Set[(Int, Int)] = Set((7,1),
                                                  //|  (7,5), (7,6), (10,3), (7,9), (9,10), (8,10), (10,5), (3,9), (3,10), (6,10)
                                                  //| , (7,4), (5,1), (7,7), (9,1), (6,1), (8,1), (0,3), (4,4), (5,10), (1,6), (2
                                                  //| ,8), (1,1), (7,3), (1,10), (4,6), (1,9), (1,4), (4,8), (3,3), (1,7), (4,3),
                                                  //|  (8,7), (9,4), (10,8))
  /*
  
 
 In IBM format, the 3x3 grid...
 381
 79c
 26c
Here the point (1,2) has the trust relationship '7' 7', i.e., 0111, meaning it depends on (2,2) (right) and (1,1) (down), but not on (1,3) above
 (since it is 0) nor on (0,2) to the left.
 
 So looks like (1,1) is the lower left, so it's 1-indexed, and goes (horizontal, vertical).
 
 In my format, I count (vertical, horizontal), 0-indexed, starting from top left.
 So the horizonal component just needs to be incremented by 1.
 The vertical component needs to be inverted. If mine goes from 0 to 11 (inclusive), and I want those to go from 12 to 1
 I'm looking at 12-v.
  
*/
  val mappedToIBMFormat = originalSolution.map{ case (a,b) => (b+1,12-a) }
                                                  //> mappedToIBMFormat  : scala.collection.immutable.Set[(Int, Int)] = Set((7,5)
                                                  //| , (2,5), (9,10), (10,5), (11,7), (4,12), (11,6), (5,11), (7,8), (11,4), (6,
                                                  //| 2), (8,11), (6,5), (11,3), (9,8), (4,5), (2,6), (4,9), (7,11), (11,9), (11,
                                                  //| 11), (5,5), (4,8), (10,11), (2,7), (4,2), (2,4), (10,9), (8,4), (5,3), (2,1
                                                  //| 1), (5,8), (2,3), (8,5), (9,2))
  
  //val originalSolution = Set((3,4), (1,8), (0,4), (3,2), (1,0), (0,10), (3,6), (0,8), (0,6), (3,10), (5,6), (2,2), (5,4), (4,8), (6,10), (4,2), (4,4), (2,0), (2,6), (6,0), (6,2), (1,2), (1,4), (4,10), (5,8), (4,6), (5,2), (5,10), (1,6), (3,8), (3,0), (2,4), (2,10), (1,10), (6,4), (2,8), (4,0), (0,0), (5,0), (6,8), (6,6), (0,2))

    

  val connectionsMatrix = connectionsAscii.map(_.split("").map(x => BigInt(x, 16)).map(x => x.toString(2)).map(y => y.reverse.padTo(4, '0').reverse).map(x => x.toList))
                                                  //> connectionsMatrix  : List[Array[List[Char]]] = List(Array(List(0, 0, 0, 0),
                                                  //|  List(1, 0, 1, 0), List(1, 0, 0, 0), List(0, 0, 1, 1), List(0, 0, 0, 0), Li
                                                  //| st(0, 0, 0, 1), List(1, 0, 1, 1), List(0, 0, 0, 1), List(0, 0, 0, 1), List(
                                                  //| 1, 0, 1, 1), List(0, 0, 0, 0), List(0, 0, 0, 1)), Array(List(0, 0, 0, 1), L
                                                  //| ist(1, 0, 1, 1), List(1, 1, 0, 1), List(1, 0, 1, 0), List(0, 1, 0, 0), List
                                                  //| (0, 0, 0, 1), List(1, 0, 1, 1), List(0, 0, 1, 0), List(0, 1, 0, 0), List(1,
                                                  //|  1, 0, 1), List(0, 1, 1, 1), List(1, 0, 0, 0)), Array(List(0, 0, 1, 1), Lis
                                                  //| t(0, 1, 1, 1), List(1, 1, 0, 0), List(0, 0, 0, 0), List(1, 0, 0, 1), List(1
                                                  //| , 1, 1, 0), List(1, 0, 0, 0), List(1, 1, 0, 1), List(0, 1, 0, 1), List(1, 0
                                                  //| , 0, 1), List(1, 0, 0, 1), List(1, 0, 0, 0)), Array(List(0, 1, 1, 0), List(
                                                  //| 0, 0, 0, 0), List(0, 1, 0, 0), List(0, 1, 1, 1), List(0, 0, 1, 1), List(0, 
                                                  //| 0, 1, 0), List(1, 0, 0, 0), List(0, 0, 1, 1), List(1, 1, 0, 1), List(0, 0, 
                                                  //| 1, 1), List(1, 0, 1, 1)
                                                  //| Output exceeds cutoff limit.
/*
 val chickenAndEggSets = (for{
   i <- 0 until 12
   j <- 0 until 12
  } yield (Set( (i,j),(i,j+1)).filter(( j < 11 && connectionsMatrix(i)(j)(1)  != '0' && connectionsMatrix(i)(j+1)(3) != '0') ++
      Set( (i,j), (i+1,j))).filter( i < 11 && connectionsMatrix(i)(j)(2) != '0' && connectionsMatrix(i+1)(j)(0) != '0')
*/

  val chickenAndEggSetsH /*: IndexedSeq[ Set[(Int,Int)]] */ = {
    for {
      i <- 0 until 12
      j <- 0 until 12
      if (j < 11 && connectionsMatrix(i)(j)(1) != '0' && connectionsMatrix(i)(j + 1)(3) != '0')
    } yield Set((i, j), (i, j + 1))               //> chickenAndEggSetsH  : scala.collection.immutable.IndexedSeq[scala.collectio
                                                  //| n.immutable.Set[(Int, Int)]] = Vector(Set((1,4), (1,5)), Set((1,8), (1,9)),
                                                  //|  Set((1,9), (1,10)), Set((2,7), (2,8)), Set((2,8), (2,9)), Set((3,2), (3,3)
                                                  //| ), Set((3,3), (3,4)), Set((3,8), (3,9)), Set((4,3), (4,4)), Set((4,6), (4,7
                                                  //| )), Set((4,8), (4,9)), Set((5,1), (5,2)), Set((5,10), (5,11)), Set((6,1), (
                                                  //| 6,2)), Set((6,10), (6,11)), Set((7,0), (7,1)), Set((7,2), (7,3)), Set((7,4)
                                                  //| , (7,5)), Set((7,7), (7,8)), Set((7,8), (7,9)), Set((8,0), (8,1)), Set((8,1
                                                  //| ), (8,2)), Set((8,6), (8,7)), Set((8,7), (8,8)), Set((8,10), (8,11)), Set((
                                                  //| 9,3), (9,4)), Set((9,4), (9,5)), Set((10,2), (10,3)), Set((10,3), (10,4)), 
                                                  //| Set((10,4), (10,5)))
  } //.flatten

  val chickenAndEggSetsV /*: IndexedSeq[ Set[(Int,Int)]] */ = {
    for {
      i <- 0 until 12
      j <- 0 until 12
      if (i < 11 && connectionsMatrix(i)(j)(2) != '0' && connectionsMatrix(i + 1)(j)(0) != '0')
    } yield Set((i, j), (i + 1, j))               //> chickenAndEggSetsV  : scala.collection.immutable.IndexedSeq[scala.collectio
                                                  //| n.immutable.Set[(Int, Int)]] = Vector(Set((0,1), (1,1)), Set((0,3), (1,3)),
                                                  //|  Set((0,6), (1,6)), Set((0,9), (1,9)), Set((1,6), (2,6)), Set((1,7), (2,7))
                                                  //| , Set((1,10), (2,10)), Set((3,4), (4,4)), Set((3,9), (4,9)), Set((3,10), (4
                                                  //| ,10)), Set((4,3), (5,3)), Set((4,10), (5,10)), Set((5,1), (6,1)), Set((6,3)
                                                  //| , (7,3)), Set((6,9), (7,9)), Set((6,10), (7,10)), Set((7,1), (8,1)), Set((7
                                                  //| ,4), (8,4)), Set((7,5), (8,5)), Set((7,6), (8,6)), Set((9,1), (10,1)), Set(
                                                  //| (9,4), (10,4)), Set((9,5), (10,5)), Set((9,10), (10,10)), Set((10,5), (11,5
                                                  //| )), Set((10,8), (11,8)))
  } //.flatten

  val chickenAndEggSets = chickenAndEggSetsH ++ chickenAndEggSetsV
                                                  //> chickenAndEggSets  : scala.collection.immutable.IndexedSeq[scala.collection
                                                  //| .immutable.Set[(Int, Int)]] = Vector(Set((1,4), (1,5)), Set((1,8), (1,9)), 
                                                  //| Set((1,9), (1,10)), Set((2,7), (2,8)), Set((2,8), (2,9)), Set((3,2), (3,3))
                                                  //| , Set((3,3), (3,4)), Set((3,8), (3,9)), Set((4,3), (4,4)), Set((4,6), (4,7)
                                                  //| ), Set((4,8), (4,9)), Set((5,1), (5,2)), Set((5,10), (5,11)), Set((6,1), (6
                                                  //| ,2)), Set((6,10), (6,11)), Set((7,0), (7,1)), Set((7,2), (7,3)), Set((7,4),
                                                  //|  (7,5)), Set((7,7), (7,8)), Set((7,8), (7,9)), Set((8,0), (8,1)), Set((8,1)
                                                  //| , (8,2)), Set((8,6), (8,7)), Set((8,7), (8,8)), Set((8,10), (8,11)), Set((9
                                                  //| ,3), (9,4)), Set((9,4), (9,5)), Set((10,2), (10,3)), Set((10,3), (10,4)), S
                                                  //| et((10,4), (10,5)), Set((0,1), (1,1)), Set((0,3), (1,3)), Set((0,6), (1,6))
                                                  //| , Set((0,9), (1,9)), Set((1,6), (2,6)), Set((1,7), (2,7)), Set((1,10), (2,1
                                                  //| 0)), Set((3,4), (4,4)), Set((3,9), (4,9)), Set((3,10), (4,10)), Set((4,3), 
                                                  //| (5,3)), Set((4,10), (5,
                                                  //| Output exceeds cutoff limit.

  println("Con43" + connectionsMatrix(4)(3))      //> Con43List(0, 1, 1, 1)
  connectionsMatrix(5)(3)                         //> res0: List[Char] = List(1, 0, 1, 1)
  println("cae " + chickenAndEggSets)             //> cae Vector(Set((1,4), (1,5)), Set((1,8), (1,9)), Set((1,9), (1,10)), Set((2
                                                  //| ,7), (2,8)), Set((2,8), (2,9)), Set((3,2), (3,3)), Set((3,3), (3,4)), Set((
                                                  //| 3,8), (3,9)), Set((4,3), (4,4)), Set((4,6), (4,7)), Set((4,8), (4,9)), Set(
                                                  //| (5,1), (5,2)), Set((5,10), (5,11)), Set((6,1), (6,2)), Set((6,10), (6,11)),
                                                  //|  Set((7,0), (7,1)), Set((7,2), (7,3)), Set((7,4), (7,5)), Set((7,7), (7,8))
                                                  //| , Set((7,8), (7,9)), Set((8,0), (8,1)), Set((8,1), (8,2)), Set((8,6), (8,7)
                                                  //| ), Set((8,7), (8,8)), Set((8,10), (8,11)), Set((9,3), (9,4)), Set((9,4), (9
                                                  //| ,5)), Set((10,2), (10,3)), Set((10,3), (10,4)), Set((10,4), (10,5)), Set((0
                                                  //| ,1), (1,1)), Set((0,3), (1,3)), Set((0,6), (1,6)), Set((0,9), (1,9)), Set((
                                                  //| 1,6), (2,6)), Set((1,7), (2,7)), Set((1,10), (2,10)), Set((3,4), (4,4)), Se
                                                  //| t((3,9), (4,9)), Set((3,10), (4,10)), Set((4,3), (5,3)), Set((4,10), (5,10)
                                                  //| ), Set((5,1), (6,1)), Set((6,3), (7,3)), Set((6,9), (7,9)), Set((6,10), (7,
                                                  //| 10)), Set((7,1), (8,1))
                                                  //| Output exceeds cutoff limit.
  println("cae3 " + chickenAndEggSets.flatten)    //> cae3 Vector((1,4), (1,5), (1,8), (1,9), (1,9), (1,10), (2,7), (2,8), (2,8),
                                                  //|  (2,9), (3,2), (3,3), (3,3), (3,4), (3,8), (3,9), (4,3), (4,4), (4,6), (4,7
                                                  //| ), (4,8), (4,9), (5,1), (5,2), (5,10), (5,11), (6,1), (6,2), (6,10), (6,11)
                                                  //| , (7,0), (7,1), (7,2), (7,3), (7,4), (7,5), (7,7), (7,8), (7,8), (7,9), (8,
                                                  //| 0), (8,1), (8,1), (8,2), (8,6), (8,7), (8,7), (8,8), (8,10), (8,11), (9,3),
                                                  //|  (9,4), (9,4), (9,5), (10,2), (10,3), (10,3), (10,4), (10,4), (10,5), (0,1)
                                                  //| , (1,1), (0,3), (1,3), (0,6), (1,6), (0,9), (1,9), (1,6), (2,6), (1,7), (2,
                                                  //| 7), (1,10), (2,10), (3,4), (4,4), (3,9), (4,9), (3,10), (4,10), (4,3), (5,3
                                                  //| ), (4,10), (5,10), (5,1), (6,1), (6,3), (7,3), (6,9), (7,9), (6,10), (7,10)
                                                  //| , (7,1), (8,1), (7,4), (8,4), (7,5), (8,5), (7,6), (8,6), (9,1), (10,1), (9
                                                  //| ,4), (10,4), (9,5), (10,5), (9,10), (10,10), (10,5), (11,5), (10,8), (11,8)
                                                  //| )

  println("cao2 " + chickenAndEggSets.flatten.contains((5, 3)))
                                                  //> cao2 true

  val biConnectionMatrix = connectionsMatrix.zipWithIndex.map {
    case (line, y) =>
      line.zipWithIndex.map {
        case (cell, x) =>
          cell.zipWithIndex.map {
            case (boolean, i) =>
              i match {
                case 0 => boolean != '0' || (y > 0 && connectionsMatrix(y - 1)(x)(2) != '0')
                case 1 => boolean != '0' || (x < 11 && connectionsMatrix(y)(x + 1)(3) != '0')
                case 2 => boolean != '0' || (y < 11 && connectionsMatrix(y + 1)(x)(0) != '0')
                case 3 => boolean != '0' || (x > 0 && connectionsMatrix(y)(x - 1)(1) != '0')
              }
          }
      }
  }                                               //> biConnectionMatrix  : List[Array[List[Boolean]]] = List(Array(List(false, f
                                                  //| alse, false, false), List(true, false, true, false), List(true, true, true,
                                                  //|  false), List(false, false, true, true), List(false, true, false, false), L
                                                  //| ist(false, true, false, true), List(true, true, true, true), List(false, tr
                                                  //| ue, false, true), List(false, true, false, true), List(true, false, true, t
                                                  //| rue), List(false, true, false, false), List(false, false, true, true)), Arr
                                                  //| ay(List(false, true, false, true), List(true, true, true, true), List(true,
                                                  //|  true, true, true), List(true, false, true, true), List(false, true, true, 
                                                  //| false), List(false, true, true, true), List(true, false, true, true), List(
                                                  //| false, false, true, false), List(false, true, false, false), List(true, tru
                                                  //| e, true, true), List(false, true, true, true), List(true, false, true, true
                                                  //| )), Array(List(false, true, true, true), List(true, true, true, true), List
                                                  //| (true, true, false, tru
                                                  //| Output exceeds cutoff limit.
  (0 until 12).foreach { i =>
    println((0 until 12).map(j => if (biConnectionMatrix(i)(j)(0)) 'X' else '.').mkString)
  }                                               //> .XX...X..X..
                                                  //| .XXX..X..X.X
                                                  //| .XXXXXXX.XXX
                                                  //| XX...XX.X.XX
                                                  //| X..XXX.XXXXX
                                                  //| .XXXX.XX..X.
                                                  //| XX.XXXX.XXX.
                                                  //| .XXXXX...XXX
                                                  //| .X.XXXXXX..X
                                                  //| .XXXXXXX....
                                                  //| XXXXXXX.XXXX
                                                  //| XXXXXXXXX.X.
  (0 until 12).foreach { i =>
    println((0 until 12).map(j => if (biConnectionMatrix(i)(j)(1)) 'X' else '.').mkString)
  }                                               //> ..X.XXXXX.X.
                                                  //| XXX.XX..XXX.
                                                  //| XXXX.XXXXX..
                                                  //| X.XX..XXXX..
                                                  //| X.XX..XXXX.X
                                                  //| XXXXXXX.X.X.
                                                  //| XX...X...XX.
                                                  //| X.X.XX.XXX..
                                                  //| XXXX.XXXXXXX
                                                  //| X.XXXXX.XX.X
                                                  //| X.XXX.XXXXX.
                                                  //| .XXX.X..X...
  (0 until 12).foreach { i =>
    println((0 until 12).map(j => if (biConnectionMatrix(i)(j)(2)) 'X' else '.').mkString)
  }                                               //> .XXX..X..X.X
                                                  //| .XXXXXXX.XXX
                                                  //| XX...XX.X.XX
                                                  //| X..XXX.XXXXX
                                                  //| .XXXX.XX..X.
                                                  //| XX.XXXX.XXX.
                                                  //| .XXXXX...XXX
                                                  //| .X.XXXXXX..X
                                                  //| .XXXXXXX....
                                                  //| XXXXXXX.XXXX
                                                  //| XXXXXXXXX.X.
                                                  //| X.....X.X.X.
  (0 until 12).foreach { i =>
    println((0 until 12).map(j => if (biConnectionMatrix(i)(j)(3)) 'X' else '.').mkString)
  }                                               //> ...X.XXXXX.X
                                                  //| XXXX.XX..XXX
                                                  //| XXXXX.XXXXX.
                                                  //| .X.XX..XXXX.
                                                  //| XX.XX..XXXX.
                                                  //| XXXXXXXX.X.X
                                                  //| XXX...X...XX
                                                  //| XX.X.XX.XXX.
                                                  //| XXXXX.XXXXXX
                                                  //| .X.XXXXX.XX.
                                                  //| XX.XXX.XXXXX
                                                  //| ..XXX.X..X..

  List(true, false).exists(x => x)                //> res1: Boolean = true

  (0 until 12).foreach { i =>
    println((0 until 12).map(j => if (biConnectionMatrix(i)(j).exists(x => x)) 'X' else '.').mkString)
  }                                               //> .XXXXXXXXXXX
                                                  //| XXXXXXXXXXXX
                                                  //| XXXXXXXXXXXX
                                                  //| XXXXXXXXXXXX
                                                  //| XXXXXXXXXXXX
                                                  //| XXXXXXXXXXXX
                                                  //| XXXXXXX.XXXX
                                                  //| XXXXXXXXXXXX
                                                  //| XXXXXXXXXXXX
                                                  //| XXXXXXXXXXXX
                                                  //| XXXXXXXXXXXX
                                                  //| XXXXXXXXXXX.

  connectionsMatrix(0)(0)                         //> res2: List[Char] = List(0, 0, 0, 0)
  // This could be an iterator, to go a bit faster
  def neighbours(person: (Int, Int)): Set[(Int, Int)] = connectionsMatrix(person._1)(person._2).zipWithIndex
    .filter(_._1 != '0') // careful because the entries are Chars, not Ints or Booleans
    .map {
      case (boolean, i) =>
        i match {
          case 0 => (person._1 - 1, person._2)
          case 1 => (person._1, person._2 + 1)
          case 2 => (person._1 + 1, person._2)
          case 3 => (person._1, person._2 - 1)
        }
    }.toSet                                       //> neighbours: (person: (Int, Int))Set[(Int, Int)]

  def neighboursBi(person: (Int, Int)): Set[(Int, Int)] = biConnectionMatrix(person._1)(person._2).zipWithIndex
    .filter(_._1) // careful because the entries are Chars, not Ints or Booleans
    .map {
      case (boolean, i) =>
        i match {
          case 0 => (person._1 - 1, person._2)
          case 1 => (person._1, person._2 + 1)
          case 2 => (person._1 + 1, person._2)
          case 3 => (person._1, person._2 - 1)
        }
    }.toSet                                       //> neighboursBi: (person: (Int, Int))Set[(Int, Int)]

  def printSVG(unvaccinated: Set[(Int, Int)], svgFileNumber: Int) = {
    val pw = new PrintWriter(new File("%07d".format(svgFileNumber) + ".svg"))
    pw.write("<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" viewBox=\"-0.5 -0.5 13 13\" height=\"" + 800 + "\" width=\"" + 800 + "\">\n")
    pw.write("<defs>\n")
    pw.write("  <marker id=\"arrowhead\" markerWidth=\"20\" markerHeight=\"14\"\n")
    pw.write("   refX=\"0\" refY=\"7\" orient=\"auto\">\n")
    pw.write("    <polygon points=\"0 0, 20 7, 0 14\" />\n")
    pw.write("  </marker>\n")
    pw.write("</defs>\n")
    pw.write("<style>\n")
    pw.write("  .small { font: italic 0.1px sans-serif; }\n")
    pw.write("</style>\n")

    /*  pw.write("<line x1=\"0\" y1=\"50\" x2=\"250\" y2=\"50\" stroke=\"#000\"\n")
  pw.write("stroke-width=\"8\" marker-end=\"url(#arrowhead)\" />\n")
 */
    /* (0 until 12).foreach{ i =>
   println((0 until 12).map( j => if (unvaccinated.contains((i,j))) 'X' else '.').mkString)}
  */

    def node(i: Int, j: Int, unvaccinated: Set[(Int, Int)]): Unit = {
      val colour: String = if ((originalSolution -- unvaccinated).contains((i,j))) "yellow"
      else
      if (!unvaccinated.contains((i, j))) "grey"
      else if (chickenAndEggSets.flatten.contains((i, j))) "green"
      else "blue"

      val bin: List[Char] = connectionsMatrix(i)(j)
      // draw up to 4 lines
      val circleLine: String = "<circle cx=\"" + j + "\" cy=\"" + i + "\" r=\"0.4\" stroke=\"black\" stroke-width=\"0.03\" fill=\"" + colour + "\" />\n"
      pw.write(circleLine)
      val textLines: String = "<text x=\"" + { j - 0.1 } + "\" y=\"" + { i - 0.1 } + "\" class=\"small\">" + connectionsAscii(i)(j) + "</text>\"\n"
      val textLines2: String = "<text x=\"" + { j - 0.1 } + "\" y=\"" + { i + 0.0 } + "\" class=\"small\">" + connectionsMatrix(i)(j).mkString + "</text>\"\n"
      val textLines3: String = "<text x=\"" + { j - 0.1 } + "\" y=\"" + { i + 0.1 } + "\" class=\"small\">(" + i + "," + j + ")</text>\"\n"
      pw.write(textLines)
      pw.write(textLines2)
      pw.write(textLines3)
    }
    def arrows(i: Int, j: Int, unvaccinated: Set[(Int, Int)]): Unit = {
      def arrowString(fromi: Double, fromj: Double, toi: Double, toj: Double): String =
        "<line stroke-opacity=\"0.7\" x1=\"" + fromi + "\" y1=\"" + fromj + "\" x2=\"" + toi + "\" y2=\"" + toj + "\" stroke=\"#000\"\n" +
          "stroke-width=\".01\" marker-end=\"url(#arrowhead)\" />\n"

      val arrowStrings = connectionsMatrix(i)(j).zipWithIndex.filter(_._1 != '0').map {
        case (boolean, in) =>
          in match {
            case 0 => arrowString(j - .1, i - .3, j - .1, i - .7)
            case 1 => arrowString(j + .3, i - .1, j + .7, i - .1)
            case 2 => arrowString(j + .1, i + .3, j + .1, i + .7)
            case 3 => arrowString(j - .3, i + .1, j - .7, i + .1)
          }
      }

      arrowStrings.foreach(pw.write)

    }
    val allStrings = for {
      i <- 0 until 12
      j <- 0 until 12
    } yield node(i, j, unvaccinated)

    for {
      i <- 0 until 12
      j <- 0 until 12
    } yield arrows(i, j, unvaccinated)

    pw.write("</svg>")
    pw.close
    //   allStrings.foreach(x => x)
  }                                               //> printSVG: (unvaccinated: Set[(Int, Int)], svgFileNumber: Int)Unit

  def isThereAtLeastOneUnvaccinatedNeighbour(unvaccinated: Set[(Int, Int)]): ((Int, Int)) => Boolean =
    (person: (Int, Int)) =>
      neighbours(person) exists unvaccinated      //> isThereAtLeastOneUnvaccinatedNeighbour: (unvaccinated: Set[(Int, Int)])((In
                                                  //| t, Int)) => Boolean

  def areAllTrustedNeighboursVaccinated(person: (Int, Int), vaccinated: Set[(Int, Int)]): Boolean =
    neighbours(person) forall vaccinated          //> areAllTrustedNeighboursVaccinated: (person: (Int, Int), vaccinated: Set[(In
                                                  //| t, Int)])Boolean

  def simpleRun(unvaccinated: Set[(Int, Int)]): Set[(Int, Int)] = {
    val (remainUnvaccinated, goingToBeVaccinated) = unvaccinated.partition(isThereAtLeastOneUnvaccinatedNeighbour(unvaccinated))
    if (goingToBeVaccinated.size == 0)
      unvaccinated
    else
      simpleRun(remainUnvaccinated)
  }                                               //> simpleRun: (unvaccinated: Set[(Int, Int)])Set[(Int, Int)]

  // We'll start this running with the newlyVaccinated Set as a single person.
  // We'll consider his neighbours, see who amoungst them get the vaccine.
  // In general we'll only consider the neighbours of the newlyVaccinated, and see who among them are converts.
  /*def runUntilNoFurtherProgress( unvaccinated: Set[(Int,Int)], newlyVaccinated: Set[(Int,Int)]) : Set[(Int,Int)] = {
  val neighboursOfNewlyVaccinated = newlyVaccinated.flatMap(neighbours)
  //val (neighouboursWhoWillRemainUnvaccinated, neighboursWhoAregettingVaccinated) =
  // (neighboursOfNewlyVaccinated intersect (unvaccinated -- newlyVaccinated)) partition isThereAtLeastOneUnvaccinatedNeighbour(unvaccinated -- newlyVaccinated)
    val neighboursWhoAregettingVaccinated =
    (unvaccinated -- newlyVaccinated) filterNot isThereAtLeastOneUnvaccinatedNeighbour(unvaccinated -- newlyVaccinated)
  if (neighboursWhoAregettingVaccinated.size == 0)
   unvaccinated -- newlyVaccinated
  else
   runUntilNoFurtherProgress(unvaccinated -- neighboursWhoAregettingVaccinated -- newlyVaccinated, neighboursWhoAregettingVaccinated)
 }
 val startingSetWeGetForFree = runUntilNoFurtherProgress( allPeople, allPeople.filterNot( isThereAtLeastOneUnvaccinatedNeighbour(allPeople)))

*/
  /*
 for everychicken and egg pair
 we must select at least 1 to the starter set.
 So, 3 cases.
 A
 B
 A and B.
 We can run them all and see if they return the same set.
 If so, it's irrelevant which we choose. So just choose A for the sake of argument.
 */

  /*  val chickenAndEggSetsWeCanChooseOneArbitrarily = chickenAndEggSets.filter{ cae =>
   val listOfChickenAndEggs = cae.toList
   val A = listOfChickenAndEggs(0)
   val B = listOfChickenAndEggs(1)
   val caseA = runUntilNoFurtherProgress(startingSetWeGetForFree, Set(A))
   val caseB = runUntilNoFurtherProgress(startingSetWeGetForFree, Set(B))
   val caseAandB = runUntilNoFurtherProgress(startingSetWeGetForFree, Set(A,B))
   caseA != startingSetWeGetForFree && caseA == caseB && caseA == caseAandB
  }

 chickenAndEggSetsWeCanChooseOneArbitrarily.size

 val startingSetWeGetForFreeWithChickensRemoved = runUntilNoFurtherProgress(startingSetWeGetForFree,
  chickenAndEggSetsWeCanChooseOneArbitrarily.map(x => x.head).toSet )

  // **** end of 1 round

  val chickenAndEggSetsWeCanChooseOneArbitrarily2 = chickenAndEggSets.filter{ cae =>
   val listOfChickenAndEggs = cae.toList
   val A = listOfChickenAndEggs(0)
   val B = listOfChickenAndEggs(1)
   val caseA = runUntilNoFurtherProgress(startingSetWeGetForFreeWithChickensRemoved, Set(A))
   val caseB = runUntilNoFurtherProgress(startingSetWeGetForFreeWithChickensRemoved, Set(B))
   val caseAandB = runUntilNoFurtherProgress(startingSetWeGetForFreeWithChickensRemoved, Set(A,B))
   caseA != startingSetWeGetForFreeWithChickensRemoved && caseA == caseB && caseA == caseAandB
  }

 chickenAndEggSetsWeCanChooseOneArbitrarily2.size

 val startingSetWeGetForFreeWithChickensRemoved2 = runUntilNoFurtherProgress(startingSetWeGetForFreeWithChickensRemoved,
  chickenAndEggSetsWeCanChooseOneArbitrarily2.map(x => x.head).toSet )
*/
  //*** end of round 2.

  //How about we put this trick in our main solver???!!! If it gets stuck. Make a guess. Then try again
  // Find a chicken and egg pair. If we can select one of them, and it dominates the other.
  def chickenAndEggSetsWeCanChooseOneArbitrarily(unvaccinated: Set[(Int, Int)]) =
    for {
      cae <- chickenAndEggSets
      one <- cae
      if ({
        val caseA = simpleRun(unvaccinated - one)
        (cae - one).forall(other => caseA.subsetOf(simpleRun(unvaccinated - other)))
      })
    } yield (one, cae - one)                      //> chickenAndEggSetsWeCanChooseOneArbitrarily: (unvaccinated: Set[(Int, Int)]
                                                  //| )scala.collection.immutable.IndexedSeq[((Int, Int), scala.collection.immut
                                                  //| able.Set[(Int, Int)])]

  //printSVG(startingSetWeGetForFree, 1)
  // printSVG(startingSetWeGetForFreeWithChickensRemoved, 2)
  // printSVG(startingSetWeGetForFreeWithChickensRemoved2, 3)

  printUnvaccinated(allPeople)                    //> XXXXXXXXXXXX
                                                  //| XXXXXXXXXXXX
                                                  //| XXXXXXXXXXXX
                                                  //| XXXXXXXXXXXX
                                                  //| XXXXXXXXXXXX
                                                  //| XXXXXXXXXXXX
                                                  //| XXXXXXXXXXXX
                                                  //| XXXXXXXXXXXX
                                                  //| XXXXXXXXXXXX
                                                  //| XXXXXXXXXXXX
                                                  //| XXXXXXXXXXXX
                                                  //| XXXXXXXXXXXX
  allPeople.size                                  //> res3: Int = 144
  //printUnvaccinated(startingSetWeGetForFree)
  //startingSetWeGetForFree.size
  implicit def hex2int(hex: String): Int = Integer.parseInt(hex, 16)
                                                  //> hex2int: (hex: String)Int

  connectionsAscii.head.toCharArray()             //> res4: Array[Char] = Array(0, a, 8, 3, 0, 1, b, 1, 1, b, 0, 1)
  connectionsAscii.head.toCharArray()(1)          //> res5: Char = a
  connectionsAscii.head.split("")                 //> res6: Array[String] = Array(0, a, 8, 3, 0, 1, b, 1, 1, b, 0, 1)
  connectionsAscii.head.split("").map(hex2int)    //> res7: Array[Int] = Array(0, 10, 8, 3, 0, 1, 11, 1, 1, 11, 0, 1)
  connectionsAscii.head.split("").map(x => BigInt(x, 16))
                                                  //> res8: Array[scala.math.BigInt] = Array(0, 10, 8, 3, 0, 1, 11, 1, 1, 11, 0,
                                                  //|  1)
  connectionsAscii.map(_.toString).map(_.sliding(1).toList)
                                                  //> res9: List[List[String]] = List(List(0, a, 8, 3, 0, 1, b, 1, 1, b, 0, 1), 
                                                  //| List(1, b, d, a, 4, 1, b, 2, 4, d, 7, 8), List(3, 7, c, 0, 9, e, 8, d, 5, 
                                                  //| 9, 9, 8), List(6, 0, 4, 7, 3, 2, 8, 3, d, 3, b, 8), List(1, 3, 2, 7, 9, 0,
                                                  //|  4, 3, d, 9, b, c), List(3, 7, 1, b, f, 4, c, 0, 2, 1, c, 1), List(1, d, 1
                                                  //| , 2, 2, e, 8, 0, 0, e, e, 1), List(5, b, c, 9, 6, 7, 2, 6, 5, d, 8, 8), Li
                                                  //| st(5, f, 1, 9, 9, 8, f, 5, 9, 1, 5, d), List(6, 2, 8, d, f, f, 0, 9, 4, 0,
                                                  //|  3, 4), List(3, 9, e, f, f, b, e, 6, e, c, c, 8), List(2, c, 4, 4, 0, c, 2
                                                  //| , 0, e, 0, a, 0))
  connectionsAscii.map(_.toString) /*.split("")*/ .map(x => BigInt(x, 16)).map(x => x.toString(2).padTo(4, '0'))
                                                  //> res10: List[String] = List(10101000001100000001101100010001101100000001, 1
                                                  //| 10111101101001000001101100100100110101111000, 1101111100000010011110100011
                                                  //| 010101100110011000, 11000000100011100110010100000111101001110111000, 10011
                                                  //| 0010011110010000010000111101100110111100, 11011100011011111101001100000000
                                                  //| 10000111000001, 111010001001000101110100000000000111011100001, 10110111100
                                                  //| 100101100111001001100101110110001000, 101111100011001100110001111010110010
                                                  //| 00101011101, 11000101000110111111111000010010100000000110100, 111001111011
                                                  //| 1111111011111001101110110011001000, 10110001000100000011000010000011100000
                                                  //| 10100000)
  connectionsAscii.head.split("").map(x => BigInt(x, 16)).map(x => x.toString(2)) //.map(x => x.toList)
                                                  //> res11: Array[String] = Array(0, 1010, 1000, 11, 0, 1, 1011, 1, 1, 1011, 0,
                                                  //|  1)
  connectionsAscii.head.split("").map(x => BigInt(x, 16)).map(x => x.toString(2)).map(y => y.reverse.padTo(4, '0').reverse).map(x => x.toList)
                                                  //> res12: Array[List[Char]] = Array(List(0, 0, 0, 0), List(1, 0, 1, 0), List(
                                                  //| 1, 0, 0, 0), List(0, 0, 1, 1), List(0, 0, 0, 0), List(0, 0, 0, 1), List(1,
                                                  //|  0, 1, 1), List(0, 0, 0, 1), List(0, 0, 0, 1), List(1, 0, 1, 1), List(0, 0
                                                  //| , 0, 0), List(0, 0, 0, 1))

  // The digit '1' indicates trust, the digit '0' indicates no trust. The neighbors are those people at the four points connected to
  // a given person on the grid, from the above neighbor and clockwise, i.e., the above, right, below, and left neighbors.

  //connectionsAscii.head.split("").map(hex2int).map(x => BigInt(x,16))
  val number: Int = "CAFE"                        //> number  : Int = 51966

  val hexA: Int = connectionsAscii.head.toCharArray()(1).toUpper.toHexString
                                                  //> hexA  : Int = 65
  hex2int("a")                                    //> res13: Int = 10

  // OK, so my thoughts are:
  // We can't just go by neighbours.
  // We have to consider each unvacinated person as a potential starter, one by one.

  neighbours((0, 0))                              //> res14: Set[(Int, Int)] = Set()
  neighbours((0, 1))                              //> res15: Set[(Int, Int)] = Set((-1,1), (1,1))

  //neighbours(person).forall( vaccinated ) // not sure this is the right syntax. I think the apply method returns a boolean, true if it's in there

  // So there are 3 outcomes.
  // We succeed
  // We fail
  // We do neither, so we recurse.

  // We succeeed when the unvaccinated set is empty, even if the budget is 0.
  // We fail when the unvaccinated set is not empty but we have a budget of 0, or when the (unvaccinated -- offLimits) set is empty.
  // We recurse otherwise.
  var minCounter = 0                              //> minCounter  : Int = 0
  def minimumSet(unvaccinated: Set[(Int, Int)], offLimits: Set[(Int, Int)], budget: Int): Option[Set[(Int, Int)]] =
    {
      if (minCounter % 1 == 0) {
        println("inside minimumSet. minCounter = " + minCounter)
        println("Unvaccinated size " + unvaccinated.size + " offLimits size = " + offLimits.size + " budget = " + budget)
        println("Unvaccinated")
        printUnvaccinated(unvaccinated)
        println("offlimits")
        printUnvaccinated(offLimits)
        printSVG(unvaccinated, 100000 + minCounter)
      }
      minCounter = minCounter + 1
      // All right. New approach.
      // Going to partition unvaccinated into its connected components!!!
      // Then we'll see if any of the components ( -- offLimits) are empty. If any are, there's no need to solve at all,
      // we just return None.
      // Otherwise, we can solve each component on its own, so the total run time will be the sum of the individual component's, not their product!
      // So potentially huge payoff.
      // And we can reuse the miniumSet code.

      // Breadth first search, returns all points that are reachable from any point in Frontier.
      def bfs(u: Set[(Int, Int)], marked: Set[(Int, Int)], frontier: Set[(Int, Int)]): Set[(Int, Int)] =
        {
          println("inside bfs")
          printUnvaccinated(u)
          println("marked")
          printUnvaccinated(marked)
          println("frontier")
          printUnvaccinated(frontier)
          frontier.headOption match {
            case Some(h) => {
              val newFrontier = frontier flatMap neighboursBi filter u
              bfs(u, marked ++ newFrontier ++ frontier, newFrontier -- marked -- frontier)
            }
            case None => marked
          }
        }

      // Returns a Set of components.
      def partitionIntoComponents(unvaccinated: Set[(Int, Int)]): Set[Set[(Int, Int)]] =
        {
          println("inside partitionInto Components")
          printUnvaccinated(unvaccinated)
          unvaccinated.headOption match {
            case Some(h) => {
              val firstComponent = bfs(unvaccinated, Set(), Set(h))
              partitionIntoComponents(unvaccinated -- firstComponent) + firstComponent
            }
            case None => Set()
          }
        }
      def solveSingleComponent(unvaccinated: Set[(Int, Int)], offLimits: Set[(Int, Int)], budget: Int): Option[Set[(Int, Int)]] =
        {
          println("inside solveSingleComponent. minCounter = " + minCounter)
          println("Unvaccinated size = " + unvaccinated.size)
          printUnvaccinated(unvaccinated)
          println("offlimits size = " + offLimits.size)
          printUnvaccinated(offLimits)
          println("Budget = " + budget)

          if (unvaccinated.isEmpty)
            Some(Set())
          else if (budget <= 0 || (unvaccinated -- offLimits).isEmpty)
            None
          else {
            // Our first try will be the so called Easy Chickens.
            // These are any person, who's in a chicken and egg pair, who "dominates" the other.
            // Since they are in a chicken and egg pair, one or the other must be choosen. (Per vertex cover problem).
            // If we can show that choosing A also selects (vaccinates) B, then there is no reason to choose B over A,
            // and we simply choose A and be done with it. Never consider not choosing A. This *SUPER* reduces the binary
            // tree search space.
            val easyChickens = chickenAndEggSetsWeCanChooseOneArbitrarily(unvaccinated)
            easyChickens.filter(e => (e._2 + e._1).subsetOf(unvaccinated) && !offLimits.contains(e._1)).headOption match {
              case Some(easyChicken) => {
                minimumSet(simpleRun(unvaccinated - easyChicken._1), offLimits, budget - 1) match {
                  case Some(sol) => Some(sol + easyChicken._1)
                  case None      => None // If we can't solve it with the easy chicken, we can't solve it at all.
                } // minimumSet match
              } // case Some(easyChicken)
              case None => { // There were no easy chickens available. Just choose a rando, but we'll have to do 2 cases now.
                println("No easy chickens") // Holy crap! Never gets here!!!
                val unvaccinatedToConsider = unvaccinated -- offLimits
                unvaccinatedToConsider.headOption match {
                  case None => None // No candidates remaining at all. Fail!
                  case Some(rando) => { // Return the minimum set of whether rando is in it or not
                    // case rando is in the minimum set
                    val included = minimumSet(simpleRun(unvaccinated - rando), offLimits, budget - 1)
                    // Originally we were going to play the vertex cover game, and choose either A and recurse, or exclude A and we must include
                    // all A's neighbours.
                    val neighboursOfRando = chickenAndEggSets.filter(c => c.contains(rando)).flatten.toSet - rando
                    val neighboursInOfflimits = neighboursOfRando exists offLimits
                    included match {
                      case Some(i) => {
                        // going to have to include another argument like membersinTheIncludedSetSoFar, and only excuted excluded if we're not too deep
                        val includedSize = i.size
                        val excluded = if (neighboursInOfflimits) None else minimumSet(simpleRun(unvaccinated -- neighboursOfRando) /**/ , offLimits + rando, scala.math.min(budget, includedSize))
                        excluded match {

                          // Consider the case where rando == A, and included returns {B,C,D}.
                          // If excluded also returns {B,C,D} then excluded is better because included really is size 4.
                          // So only return included + rando if its size is strickly less than excluded's.
                          case Some(e) => if (i.size < (e ++ neighboursOfRando).size) Some(i + rando) else Some(e ++ neighboursOfRando /**/ )
                          case None    => Some(i + rando)
                        }
                      }
                      case None => { // So including rando (vaccinated him) failed to yield a solution. Perhaps it was due to budget.
                        val excluded = if (neighboursInOfflimits) None else minimumSet(simpleRun(unvaccinated ++ neighboursOfRando) /**/ , offLimits + rando, budget)
                        excluded match {
                          case Some(e) => Some(e ++ neighboursOfRando /**/ )
                          case None    => None
                        } // excluded match
                      } // case None
                    } // included match
                  } // Some(rando)
                } // unvaccinatedToConsider...headOption match
              } // case None
            } // easyChickenFilter headoption match
          } // else
        } // This is the end of SolveSingleComponent

      // Want to do a NAND (not and) on both elements of a chicken and egg pair.
      // Not both are in offLimits
      //if ( chickenAndEggSets.forall(x => ! (x forall offLimits)) )
      // {

      solveSingleComponent(unvaccinated, offLimits, budget)

      /*
  println("Partitioning into components... " )
  val partitions = partitionIntoComponents(unvaccinated)
  val numComponents = partitions.size
  println("We've got " + numComponents + " components.")
  // Since each component is going to require at least 1 element to solve it, the total budget is going to be at least
  // numComponets. And further, each component must now have a budget of (budget - numComponents + 1).
  // If the original budget was 5, and we have 3 components.
  // We can only have 3+1+1.
  val partitionsSolutionsStream = partitions.toStream.sortBy(_.size).map( partition => solveSingleComponent(partition, offLimits, budget - numComponents + 1))

  if (partitions.size <= budget && partitionsSolutionsStream.forall(x => x.isDefined ))
  {
   // val p = partitionsStream                                 // Stream(Some(Set(1, 2)), ?)
   // val pf = partitionsStream.flatten                        // Stream(Set(1, 2), ?)
   // val pff = partitionsStream.flatten.flatten               // Stream(1, ?)
   val pffs = partitionsSolutionsStream.flatten.flatten.toSet  // Set(1, 2, 3, 4)
   //val col = partitionsStream.collect(_)
   //val pfm = pf.
   Some(pffs)                                                  // Some(Set(1, 2, 3, 4))
  }
  else // we are over budget or one of the partitions is unsolvable
   None


   */
    } // end of MinimumSet function               //> minimumSet: (unvaccinated: Set[(Int, Int)], offLimits: Set[(Int, Int)], bu
                                                  //| dget: Int)Option[Set[(Int, Int)]]

  val testFLatten = Stream(Some(Set(1, 2)), Some(Set(3, 4)))
                                                  //> testFLatten  : scala.collection.immutable.Stream[Some[scala.collection.imm
                                                  //| utable.Set[Int]]] = Stream(Some(Set(1, 2)), ?)
  testFLatten                                     //> res16: scala.collection.immutable.Stream[Some[scala.collection.immutable.S
                                                  //| et[Int]]] = Stream(Some(Set(1, 2)), ?)
  testFLatten.forall(_.isDefined)                 //> res17: Boolean = true
  testFLatten.flatten                             //> res18: scala.collection.immutable.Stream[scala.collection.immutable.Set[In
                                                  //| t]] = Stream(Set(1, 2), ?)
  testFLatten.flatten.flatten                     //> res19: scala.collection.immutable.Stream[Int] = Stream(1, ?)
  testFLatten.flatten.flatten.toSet               //> res20: scala.collection.immutable.Set[Int] = Set(1, 2, 3, 4)
  Some(testFLatten.flatten.flatten.toSet)         //> res21: Some[scala.collection.immutable.Set[Int]] = Some(Set(1, 2, 3, 4))

  // val startingSetWeGetForFree = runUntilNoFurtherProgress( allPeople.filter( areAllTrustedNeighboursVaccinated(_, Set())), allPeople)
  def main(args: Array[String]): Unit = {
    println("Doing chicken and egg test")

    println(chickenAndEggSetsWeCanChooseOneArbitrarily(allPeople))

    println("Starting solve")
    //val m = minimumSet(startingSetWeGetForFreeWithChickensRemoved2, Set(), 144)
    //val m = minimumSet(simpleRun(allPeople), Set(), 144)
    val m = minimumSet(allPeople, Set(), 144)
    println("Solution " + m)
    printUnvaccinated(m.get)
    println("Solution has size " + m.get.size)

    printSVG(m.get, 42)
    // scalac -deprecation ponder.sc && scala ponder && mogrify -format png -resize 1080x1080 01000*.svg && ffmpeg -r 5 -i 01000%02d.png -c:v libx264 -pix_fmt yuv420p -y out.mp4 && mplayer -loop 0 out.mp4
    
    /* println("n" + neighbours( (9,11)))
 println("ascii " + connectionsAscii(9)(11))
 println("connect " +  connectionsMatrix(9)(11).zipWithIndex)
 */
    /*  .filter( _._1 != '0')  // careful because the entries are Chars, not Ints or Booleans
  .map{ case (boolean, i) =>
   i match {
    case 0 => (person._1,     person._2 - 1)
    case 1 => (person._1 + 1, person._2)
    case 2 => (person._1,     person._2 + 1)
    case 3 => (person._1 - 1, person._2)
   }
  }.toSet
*/

    /*
println("is there...(9,11) " + isThereAtLeastOneUnvaccinatedNeighbour( allPeople)((9,11)))
println("is there...(10,11) " + isThereAtLeastOneUnvaccinatedNeighbour( allPeople)((10,11)))

 var counter = 0
 val sols = for {
 a1 <- startingSetWeGetForFree.toIterator
 s1 = runUntilNoFurtherProgress(startingSetWeGetForFree, Set(a1))
 a2 <- {
 println("s1 " + s1.size)
 s1.filter( x => 12*x._1 + x._2 > 12*a1._1 + a1._2)}
 s2 = runUntilNoFurtherProgress(s1, Set(a2))
 a3 <- {
 println("s2 " + s2.size)
 s2.filter( x => 12*x._1 + x._2 > 12*a2._1 + a2._2)}
 s3 = runUntilNoFurtherProgress(s2, Set(a3))
 a4 <- {
 println("s3 " + s3.size)
 s3.filter( x => 12*x._1 + x._2 > 12*a3._1 + a3._2)}
 s4 = runUntilNoFurtherProgress(s3, Set(a4))
 a5 <- {
 println("s4 " + s4.size)
 s4.filter( x => 12*x._1 + x._2 > 12*a4._1 + a4._2)}
 s5 = runUntilNoFurtherProgress(s4, Set(a5))
 a6 <- {
// println("s5 " + s5.size)
 s5.filter( x => 12*x._1 + x._2 > 12*a5._1 + a5._2)}
 s6 = runUntilNoFurtherProgress(s5, Set(a6))
 a7 <- {
// println("s6 " + s6.size)
 s6.filter( x => 12*x._1 + x._2 > 12*a6._1 + a6._2)}
 s7 = runUntilNoFurtherProgress(s6, Set(a7))
 a8 <- {
// println("s7 " + s7.size)
// printUnvaccinated(Set(a1,a2,a3,a4,a5,a6,a7))
// printUnvaccinated(s7)
 s7.filter( x => 12*x._1 + x._2 > 12*a7._1 + a7._2)}
 s8 = runUntilNoFurtherProgress(s7, Set(a8))
// println((a1,a2,a3,a4,a5,a6,a7,a8))
 if (s8.isEmpty)
 } yield (a1,a2,a3,a4,a5,a6,a7,a8)

 sols.take(1).foreach(println)
*/

    /* var counter = 0
 val solution = startingSetWeGetForFree.subsets.find( s => {
 val trial = runUntilNoFurtherProgress(startingSetWeGetForFree, s)
 if (counter % 10000== 0 )
 {
  println(s.size)
  printUnvaccinated(s)
  println(trial.size)
  printUnvaccinated(trial)
 }

 counter = counter + 1
 trial == Set()
 })
 println("Solution " + solution.get)
 printUnvaccinated(solution.get)
*/
  }                                               //> main: (args: Array[String])Unit

}