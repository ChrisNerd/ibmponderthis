import scala.util.control.Breaks._
import sun.org.mozilla.javascript.internal.ast.ContinueStatement
import com.sun.org.apache.xalan.internal.xsltc.compiler.FloorCall
object ponder {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  val Nmax = 31                                   //> Nmax  : Int = 31
  val NumCubes = 25                               //> NumCubes  : Int = 25
  //matrix(25,Nmax)=0
  var matrix = Array.ofDim[Boolean](NumCubes, Nmax)
                                                  //> matrix  : Array[Array[Boolean]] = Array(Array(false, false, false, false, fa
                                                  //| lse, false, false, false, false, false, false, false, false, false, false, f
                                                  //| alse, false, false, false, false, false, false, false, false, false, false, 
                                                  //| false, false, false, false, false), Array(false, false, false, false, false,
                                                  //|  false, false, false, false, false, false, false, false, false, false, false
                                                  //| , false, false, false, false, false, false, false, false, false, false, fals
                                                  //| e, false, false, false, false), Array(false, false, false, false, false, fal
                                                  //| se, false, false, false, false, false, false, false, false, false, false, fa
                                                  //| lse, false, false, false, false, false, false, false, false, false, false, f
                                                  //| alse, false, false, false), Array(false, false, false, false, false, false, 
                                                  //| false, false, false, false, false, false, false, false, false, false, false,
                                                  //|  false, false, false, false, false, false, false, false, false, false, false
                                                  //| , false, false, false), 
                                                  //| Output exceeds cutoff limit.

  printmatbin(matrix)                             //> 
                                                  //| 
                                                  //| 
                                                  //| 
                                                  //| 
                                                  //| 
                                                  //| 
                                                  //| 
                                                  //| 
                                                  //| 
                                                  //| 
                                                  //| 
                                                  //| 
                                                  //| 
                                                  //| 
                                                  //| 
                                                  //| 
                                                  //| 
                                                  //| 
                                                  //| 
                                                  //| 
                                                  //| 
                                                  //| 
                                                  //| 
                                                  //| 
                                                  //| 
                                                  //| 
  //      println(matrix.deep.map(x=>if (x) 1 else 0).mkString("\n"))
  def r(matrix: Array[Array[Boolean]], cubeNumber: Int, sideIndex: Int, sideNumber: Int): Boolean =
    {
      if (cubeNumber == 20) { //NumCubes) {
        // output the mat
        //println(matrix.deep.mkString("\n"))
        printmatbin(matrix)
        true
      } else {
        if (sideIndex >= Nmax - 6 + sideNumber) {
          false
        } else {
          if (test(sideIndex, cubeNumber, matrix)) {
            matrix(cubeNumber)(sideIndex) = true
            val re =
              {
                if (sideNumber == 6) {
                  r(matrix, cubeNumber + 1, 0, 1)
                } else {
                  r(matrix, cubeNumber, sideIndex + 1, sideNumber + 1)
                }
              }
            if (re == false) {
              matrix(cubeNumber)(sideIndex) = false
              r(matrix, cubeNumber, sideIndex + 1, sideNumber)
            } else {
              true
            }
          } else // test failed try the next index
          {
            r(matrix, cubeNumber, sideIndex + 1, sideNumber)
          }
        }
      }
    }                                             //> r: (matrix: Array[Array[Boolean]], cubeNumber: Int, sideIndex: Int, sideNum
                                                  //| ber: Int)Boolean
  def printmatbin(matrix: Array[Array[Boolean]]) = {
    /*for (i <- 0 until NumCubes) {
      for (j <- 0 until Nmax) {
        if (matrix(i)(j))
          print("1")
        else
          print("0")
      }
      println("")
    }
*/
    for (i <- 0 until NumCubes) {
      for (j <- 0 until Nmax) {
        if (matrix(i)(j))
          print(j + " ")
      }
      println("")
    }
    println("\n")
  }                                               //> printmatbin: (matrix: Array[Array[Boolean]])Unit
  def test(sideIndex: Int, cubeNumber: Int, matrix: Array[Array[Boolean]]): Boolean = {
    // 25 cubes, 6 sides per cube.  Each cube needs to have a match with 24 other cubes.
    // I think this means that each of the 6 sides should match with exactly 4 other cubes.
    // So if a number already has 5 cubes, then it can't have any more.
    var nc = 0
    for (cubei <- 0 until cubeNumber) {
      if (matrix(cubei)(sideIndex))
        nc += 1
    }
    var returnvar = true
    if (nc >= 5) {
      returnvar = false
    } else {
      // get the numbers on this cube so far
      //for (ii<- filter matrix row n for entries ==1 (with zip?))
      for (ii <- 0 until sideIndex) {
        /*      if (returnvar == false) {
        break
      }*/
        if (returnvar && matrix(cubeNumber)(ii) == true) {
          // Exactly one matching number with every other cube
          // Does entry i,n make it so that any cube 0->n-1 have more than one other number less than i in common
          // Assume there has been no mistakes so far, that there is not more than 1 same number on a pair of cubes
          for (j <- 0 until cubeNumber) {
            if (matrix(j)(ii) && matrix(j)(sideIndex)) {
              returnvar = false
              //            break
            }
          }
        }
      }
    }
    returnvar
  }                                               //> test: (sideIndex: Int, cubeNumber: Int, matrix: Array[Array[Boolean]])Boole
                                                  //| an
  r(matrix, 0, 0, 1)                              //> 0 1 2 3 4 5 
                                                  //| 0 6 7 8 9 10 
                                                  //| 0 11 12 13 14 15 
                                                  //| 0 16 17 18 19 20 
                                                  //| 0 21 22 23 24 25 
                                                  //| 1 6 11 16 21 26 
                                                  //| 1 7 12 17 22 27 
                                                  //| 1 8 13 18 23 28 
                                                  //| 1 9 14 19 24 29 
                                                  //| 2 6 12 18 24 30 
                                                  //| 2 7 11 19 25 28 
                                                  //| 2 8 14 20 21 27 
                                                  //| 2 9 15 17 23 26 
                                                  //| 3 6 13 17 25 29 
                                                  //| 3 7 14 16 23 30 
                                                  //| 3 10 12 20 26 28 
                                                  //| 4 7 15 18 21 29 
                                                  //| 4 9 11 20 22 30 
                                                  //| 5 10 13 19 21 30 
                                                  //| 5 15 16 24 27 28 
                                                  //| 
                                                  //| 
                                                  //| 
                                                  //| 
                                                  //| 
                                                  //| 
                                                  //| 
                                                  //| res0: Boolean = true

  // 24 choose 5 = 42504... n=30, first 6 have 1 entry
  var cand = Array.ofDim[Boolean](42504, 24)      //> cand  : Array[Array[Boolean]] = Array(Array(false, false, false, false, fal
                                                  //| se, false, false, false, false, false, false, false, false, false, false, f
                                                  //| alse, false, false, false, false, false, false, false, false), Array(false,
                                                  //|  false, false, false, false, false, false, false, false, false, false, fals
                                                  //| e, false, false, false, false, false, false, false, false, false, false, fa
                                                  //| lse, false), Array(false, false, false, false, false, false, false, false, 
                                                  //| false, false, false, false, false, false, false, false, false, false, false
                                                  //| , false, false, false, false, false), Array(false, false, false, false, fal
                                                  //| se, false, false, false, false, false, false, false, false, false, false, f
                                                  //| alse, false, false, false, false, false, false, false, false), Array(false,
                                                  //|  false, false, false, false, false, false, false, false, false, false, fals
                                                  //| e, false, false, false, false, false, false, false, false, false, false, fa
                                                  //| lse, false), Array(fals
                                                  //| Output exceeds cutoff limit.
  //populate
  var c = 0                                       //> c  : Int = 0
  for (i1 <- 0 until (24 - 4)) {
    for (i2 <- i1 + 1 until (24 - 3)) {
      for (i3 <- i2 + 1 until (24 - 2)) {
        for (i4 <- i3 + 1 until (24 - 1)) {
          for (i5 <- i4 + 1 until (24 - 0)) {
            cand(c)(i1) = true
            cand(c)(i2) = true
            cand(c)(i3) = true
            cand(c)(i4) = true
            cand(c)(i5) = true
            c += 1
          }
        }
      }
    }
  }

  def distance(a: Array[Boolean], b: Array[Boolean]): Int = {
    var c = 0;
    for (i <- 0 until a.length) {
      if (a(i) != b(6 + i)) {
        c += 1
      }
    }
    c
  }                                               //> distance: (a: Array[Boolean], b: Array[Boolean])Int

  def isDistanceGood(can: Array[Boolean], m: Array[Array[Boolean]], starti: Int, finishi: Int): Boolean = {
    var returnval = true
    for (ii <- starti until finishi) {
    val group1 = scala.math.floor((finishi-1)/4)
    val group2 = scala.math.floor((ii-1)/4)
    val d = if(group1==group2) 10 else 8
    
      if (returnval && (distance(can, m(ii)) != d)) {
        returnval = false
      }
    }
    returnval
  }                                               //> isDistanceGood: (can: Array[Boolean], m: Array[Array[Boolean]], starti: Int
                                                  //| , finishi: Int)Boolean
  val goodCan = scala.collection.mutable.ListBuffer[Int]()
                                                  //> goodCan  : scala.collection.mutable.ListBuffer[Int] = ListBuffer()
  // for (i <- 5 until NumCubes) {
  for (j <- 0 until 42504) {
    //try all the candidates
    if (isDistanceGood(cand(j), matrix, 1, 9)) {
  //  println(j)
      goodCan.append(j)
    }

  }
  // }
  goodCan.size                                    //> res1: Int = 96

  // 96 good candidates that fit with the first 9 entries (cubes) of the matrix
  // Start by choosing 1 candidate for entry 9
  // No need to test it against the entries 0-8
  // Choose a second cand for entry 10
  // Test it against entry 9

  def r2(matrix: Array[Array[Boolean]], cubeNumber: Int, canNumber: Int): Boolean =
    {
      if (cubeNumber == 25) { // shouldb e 26 but doesnn't seem to terminate
        printmatbin(matrix)
        true
      } else {
      
      /*
          if (test(sideIndex, cubeNumber, matrix)) {
            matrix(cubeNumber)(sideIndex) = true
            val re =
              {
                if (sideNumber == 6) {
                  r(matrix, cubeNumber + 1, 0, 1)
                } else {
                  r(matrix, cubeNumber, sideIndex + 1, sideNumber + 1)
                }
              }
            if (re == false) {
              matrix(cubeNumber)(sideIndex) = false
              r(matrix, cubeNumber, sideIndex + 1, sideNumber)
            } else {
              true
            }
          } else // test failed try the next index
          {
            r(matrix, cubeNumber, sideIndex + 1, sideNumber)
          }
      
      */
      var retval = false
        for (i <- 0 until goodCan.size) {
          // check cand(goodCan(i)) against the matrix from 9 to cubeNumber
          
          // K.  The 10 needs to change.
          // from cubeNumber 9 to 12, the distance should be 10,
          // but comparing 13 to (9 through 12) the distance should be 8
          // so comparing within a group of 4 it should be 10, but out of a group of 4
          // it should be 8
          // so group number = floor((cubeNumber-1)/4)
          
          if (!retval && isDistanceGood(cand(goodCan(i)), matrix, 9, cubeNumber)) {

            // put cand(goodCan(i)) into matrix(cubeNumber) then recurse
            for (j <- 0 until 24) {
              matrix(cubeNumber)(j + 6) = cand(goodCan(i))(j)
            }
            if (r2(matrix, cubeNumber + 1, i)) {
              retval = true
            }
          }
        }
        retval
      }
    }                                             //> r2: (matrix: Array[Array[Boolean]], cubeNumber: Int, canNumber: Int)Boolean
                                                  //| 
  r2(matrix, 9, 0)                                //> 0 1 2 3 4 5 
                                                  //| 0 6 7 8 9 10 
                                                  //| 0 11 12 13 14 15 
                                                  //| 0 16 17 18 19 20 
                                                  //| 0 21 22 23 24 25 
                                                  //| 1 6 11 16 21 26 
                                                  //| 1 7 12 17 22 27 
                                                  //| 1 8 13 18 23 28 
                                                  //| 1 9 14 19 24 29 
                                                  //| 2 6 12 18 25 29 30 
                                                  //| 2 7 13 20 24 26 
                                                  //| 2 8 15 19 21 27 
                                                  //| 2 10 14 16 22 28 
                                                  //| 3 6 14 20 23 27 
                                                  //| 3 7 11 19 25 28 30 
                                                  //| 3 9 15 18 22 26 
                                                  //| 4 10 13 17 21 29 
                                                  //| 4 6 15 17 24 28 30 
                                                  //| 5 8 11 20 22 29 30 
                                                  //| 5 9 13 16 25 27 
                                                  //| 10 12 19 23 26 
                                                  //| 7 15 16 23 29 
                                                  //| 8 14 17 25 26 
                                                  //| 9 12 20 21 28 
                                                  //| 10 11 18 24 27 
                                                  //| 
                                                  //| 
                                                  //| res2: Boolean = true
}