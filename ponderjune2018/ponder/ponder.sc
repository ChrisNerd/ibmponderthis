object ponder {
// def main(args: Array[String]) = {
  val L = Set((0,0),(0,1),(1,0),(0,2))            //> L  : scala.collection.immutable.Set[(Int, Int)] = Set((0,0), (0,1), (1,0), (0
                                                  //| ,2))
  
  def multiply2X2(b: Set[(Int,Int)], m: List[(Int,Int)]) : Set[(Int,Int)] = {
   b map { case (x,y) => ( m(0)._1 * x + m(0)._2 * y , m(1)._1 * x + m(1)._2 *y) }
  }                                               //> multiply2X2: (b: Set[(Int, Int)], m: List[(Int, Int)])Set[(Int, Int)]
  val J = multiply2X2(L, List((1,0),(0,-1))) // L * V (from isSymmetric)
                                                  //> J  : Set[(Int, Int)] = Set((0,0), (0,-1), (1,0), (0,-2))
  
  def rotNTimes(b: Set[(Int,Int)], n: Int): Set[(Int,Int)] = n match {
   case 0 => b
   // !!! IS this the RIGHT ROT MATRIX?
   case _ => multiply2X2(rotNTimes(b,n-1), List( (0,1), (-1,0)))
  }                                               //> rotNTimes: (b: Set[(Int, Int)], n: Int)Set[(Int, Int)]
  
  def hasHolesR( frontier: Set[(Int,Int)], b: Set[(Int,Int)]) : Boolean = frontier.size match {
  // Are there holes
  case 0 => b.size != (b.maxBy(_._1)._1 - b.minBy(_._1)._1 + 1) *
                      (b.maxBy(_._2)._2 - b.minBy(_._2)._2 + 1)
                      
// FILTER by min and max
  case _ => hasHolesR( (frontier flatMap { case (x,y) => Set((x+1,y),(x-1,y),(x,y-1),(x,y+1)) }).filter{ case(x,y) =>
   x >= b.minBy(_._1)._1 &&
   x <= b.maxBy(_._1)._1 &&
   y >= b.minBy(_._2)._2 &&
   y <= b.maxBy(_._2)._2
   } -- b , b ++ frontier)
  }                                               //> hasHolesR: (frontier: Set[(Int, Int)], b: Set[(Int, Int)])Boolean
  
  def hasHoles(b: Set[(Int,Int)]) : Boolean = {
   val minx = b.minBy(_._1)._1
   val maxx = b.maxBy(_._1)._1
   val miny = b.minBy(_._2)._2
   val maxy = b.maxBy(_._2)._2
   
   val fro = ((minx to maxx) map (x => (x,miny))).toSet ++
   ((minx to maxx) map (x => (x,maxy))).toSet ++
   ((miny to maxy) map (y => (minx,y))).toSet ++
   ((miny to maxy) map (y => (maxx,y))).toSet
   
   hasHolesR(fro.filter{ case(x,y) =>
   x >= b.minBy(_._1)._1 &&
   x <= b.maxBy(_._1)._1 &&
   y >= b.minBy(_._2)._2 &&
   y <= b.maxBy(_._2)._2
   } -- b, b)
  }                                               //> hasHoles: (b: Set[(Int, Int)])Boolean
  
  hasHoles( Set(
  (0,0),(0,1),(0,2),
  (1,0),      (1,2),
  (2,0),(2,1),(2,2)) )                            //> res0: Boolean = true
  
  def normalize(block: Set[(Int,Int)]) = {
   val minx = block.minBy(_._1)._1
   val miny = block.minBy(_._2)._2
   block.map{ case (x,y) => (x-minx,y-miny)}
  }                                               //> normalize: (block: Set[(Int, Int)])scala.collection.immutable.Set[(Int, Int
                                                  //| )]
  
 def isSymmetric(block: Set[(Int,Int)]) : Boolean = {
  Set('H','V','\\','/') exists { sym =>
   sym match {
    case 'H' =>  normalize(block) == normalize(multiply2X2(block, List((-1,0),(0,1))))
    case 'V' =>  normalize(block) == normalize(multiply2X2(block, List((1,0),(0,-1))))
    case '\\' => normalize(block) == normalize(multiply2X2(block, List((0,-1),(-1,0))))
    case '/' =>  normalize(block) == normalize(multiply2X2(block, List((0,1),(1,0))))
   }
  }
 }                                                //> isSymmetric: (block: Set[(Int, Int)])Boolean
 
 val symT =Set( (-1,-1),
 (-2,0),
(-3,0),
(0,0),
(-3,1),
(-2,1),
(-3,-2),
(-3,-1),
(-4,-1),
(0,-2),
(-1,0),
(-2,-1),
(-1,2),
(-1,3),
(0,1),
(-1,1),
(-2,-2),
(-5,-1),
(-1,-2),
(0,-1))                                           //> symT  : scala.collection.immutable.Set[(Int, Int)] = Set((-1,-1), (-2,0), (
                                                  //| -3,0), (0,0), (-3,1), (-2,1), (-3,-2), (-3,-1), (-4,-1), (0,-2), (-1,0), (-
                                                  //| 2,-1), (-1,2), (-1,3), (0,1), (-1,1), (-2,-2), (-5,-1), (-1,-2), (0,-1))

normalize(symT)                                   //> res1: scala.collection.immutable.Set[(Int, Int)] = Set((5,0), (5,2), (5,1),
                                                  //|  (4,0), (3,1), (4,1), (2,0), (4,4), (3,0), (1,1), (4,5), (3,2), (2,2), (4,2
                                                  //| ), (0,1), (5,3), (3,3), (2,3), (2,1), (4,3))
multiply2X2(symT, List((0,-1),(-1,0)))            //> res2: Set[(Int, Int)] = Set((1,5), (0,2), (0,0), (-3,1), (-2,1), (2,0), (0,
                                                  //| 3), (1,1), (1,4), (-1,0), (1,3), (2,2), (-1,2), (-1,3), (0,1), (-1,1), (2,3
                                                  //| ), (1,2), (2,1), (1,0))
normalize(multiply2X2(symT, List((0,-1),(-1,0)))) //> res3: scala.collection.immutable.Set[(Int, Int)] = Set((5,0), (5,2), (5,1),
                                                  //|  (4,0), (3,1), (4,1), (2,0), (4,4), (3,0), (1,1), (4,5), (3,2), (2,2), (4,2
                                                  //| ), (0,1), (5,3), (3,3), (2,3), (2,1), (4,3))
normalize(symT) == normalize(multiply2X2(symT, List((0,-1),(-1,0))))
                                                  //> res4: Boolean = true
normalize(symT) == normalize(multiply2X2(symT, List((-1,0),(0,1))))
                                                  //> res5: Boolean = false
isSymmetric(symT)                                 //> res6: Boolean = true

 
 def JsPerElement( el: (Int,Int) , block: Set[(Int,Int)] ) : IndexedSeq[Set[(Int,Int)]] = {
 
 
 for {
  rotatedJ <- ((0 to 3) map { r => rotNTimes(J,r) })
  //shiftedJ <- rotatedJ map { pieceOfJ => rotatedJ map { p => (p._1 - pieceOfJ._1, p._2 - pieceOfJ._2) } }
  pieceOfJ <- rotatedJ
  val shiftedJ = rotatedJ map { p => (el._1 + p._1- pieceOfJ._1, el._2 + p._2 - pieceOfJ._2) }
  if ( block.intersect(shiftedJ) == shiftedJ)
 } yield shiftedJ
 
 
 /*
  val Js = ((0 to 3) map { r => rotNTimes(J, r) }).toSet
  val allJs = Js map { rotatedJ => rotatedJ map { pieceofJ => rotatedJ map { p => (p._1-pieceofJ._1, p._2 - pieceofJ._2) }  } }
  
  // We've got to filter!!!
  allJs.flatten.filter{  j => block.intersect(j) == j }*/
 }                                                //> JsPerElement: (el: (Int, Int), block: Set[(Int, Int)])IndexedSeq[Set[(Int, 
                                                  //| Int)]]
 
 JsPerElement( (0,0), L)                          //> res7: IndexedSeq[Set[(Int, Int)]] = Vector()
 JsPerElement( (1,0), J)                          //> res8: IndexedSeq[Set[(Int, Int)]] = Vector(Set((0,0), (0,-1), (1,0), (0,-2)
                                                  //| ))
 JsPerElement( (0,-1), J ++ Set( (1,-1), (0,-3))) //> res9: IndexedSeq[Set[(Int, Int)]] = Vector(Set((0,-1), (0,-2), (1,-1), (0,-
                                                  //| 3)), Set((0,0), (0,-1), (1,0), (0,-2)))
 
 def isConstructableByJ(block: Set[(Int,Int)]): Boolean = {
  if (block.size == 0)
   true
  else
  {
   val bMin = block.minBy( el => JsPerElement(el,block).size)
   if (JsPerElement(bMin, block).size == 0)
    false
   else
    JsPerElement(bMin, block).exists(Jblock => isConstructableByJ(block -- Jblock))
  }
 }                                                //> isConstructableByJ: (block: Set[(Int, Int)])Boolean
 
 
 isConstructableByJ(J)                            //> res10: Boolean = true
 isConstructableByJ(L)                            //> res11: Boolean = false
 
 
 
  
 // Returns a new block made out of n+1 L shaped tetrominos (Set with 4*(n+1) elements, where n is the block.size/4)
 // Since createBlob rotates both block and newJ, (accounting for all 16 combinations w.r.t. rotations) we will always slide in from right to left.
 def addL(block: Set[(Int,Int)], tToAdd: Set[(Int,Int)], offset: Int): Set[(Int,Int)]  = {
  val maxBx = block.maxBy(_._1)._1
  val mintx = tToAdd.minBy(_._1)._1
  val mint = Stream.from(0).find{ t => block.intersect( tToAdd.map{case (x,y) => (x - t + maxBx-mintx, y+offset ) } ).nonEmpty}
  mint match {
   case Some(t) => block ++ {tToAdd.map{ case (x,y) => (x + 1 - t + maxBx - mintx, y + offset) } }
   case None => {
    println("Missed")
    block
   }
  }
 }                                                //> addL: (block: Set[(Int, Int)], tToAdd: Set[(Int, Int)], offset: Int)Set[(In
                                                  //| t, Int)]
 addL(L,L,0)                                      //> res12: Set[(Int, Int)] = Set((0,2), (0,0), (2,0), (3,0), (2,2), (0,1), (2,1
                                                  //| ), (1,0))
 addL(L,L,1)                                      //> res13: Set[(Int, Int)] = Set((0,2), (0,0), (1,1), (1,3), (0,1), (1,2), (2,1
                                                  //| ), (1,0))
 addL(L,L,2)                                      //> res14: Set[(Int, Int)] = Set((0,2), (0,0), (1,4), (1,3), (2,2), (0,1), (1,2
                                                  //| ), (1,0))
 L                                                //> res15: scala.collection.immutable.Set[(Int, Int)] = Set((0,0), (0,1), (1,0)
                                                  //| , (0,2))
 rotNTimes(L, 1)                                  //> res16: Set[(Int, Int)] = Set((0,0), (1,0), (0,-1), (2,0))
 rotNTimes(L, 2)                                  //> res17: Set[(Int, Int)] = Set((0,0), (0,-1), (-1,0), (0,-2))
 
 addL(L,rotNTimes(L, 1),0)                        //> res18: Set[(Int, Int)] = Set((0,2), (0,0), (2,-1), (4,0), (2,0), (3,0), (0,
                                                  //| 1), (1,0))
 addL(L,rotNTimes(L, 2),3)                        //> res19: Set[(Int, Int)] = Set((0,2), (0,0), (0,3), (1,1), (1,3), (0,1), (1,2
                                                  //| ), (1,0))
 
 isConstructableByJ(addL(L,rotNTimes(L, 2),3))    //> res20: Boolean = true
 
 
 val maxBx = L.maxBy(_._1)._1                     //> maxBx  : Int = 1
 val mintx = L.minBy(_._1)._1                     //> mintx  : Int = 0
 val mint = Stream.from(0).find{ t => L.intersect( L.map{case (x,y) => (x + 1 -t + maxBx-mintx,y) } ).nonEmpty}
                                                  //> mint  : Option[Int] = Some(1)
 L.map{case (x,y) => (x + 1 -1 + maxBx-mintx,y) } //> res21: scala.collection.immutable.Set[(Int, Int)] = Set((1,0), (1,1), (2,0)
                                                  //| , (1,2))
                                             
 def createBlob(blocks: Iterator[Set[(Int,Int)]], depth: Int): Iterator[Set[(Int,Int)]] = {
  if (depth == 5 ) //// depth == 5 ////
   blocks
  else
   createBlob (
    for {
     block <- blocks
     Js <- (0 to 3) map { r => rotNTimes(block,r) }
     newL <- (0 to 3) map { r => rotNTimes(L, r) }
     os <- (Js.minBy(_._2)._2 - newL.maxBy(_._2)._2) to (Js.maxBy(_._2)._2 - newL.minBy(_._2)._2)
     if (!hasHoles(addL(Js, newL, os)))
    } yield addL(Js, newL, os),
   depth + 1
  )
 }                                                //> createBlob: (blocks: Iterator[Set[(Int, Int)]], depth: Int)Iterator[Set[(In
                                                  //| t, Int)]]
 
 val solutions = createBlob(Iterator(L), 1).filter(b=> isConstructableByJ(b) && !hasHoles(b) && !isSymmetric(b))
                                                  //> solutions  : Iterator[Set[(Int, Int)]] = non-empty iterator
 
 solutions foreach { s => {
 println("Solution")
  s foreach (se => println(se))}                  //> Solution
                                                  //| (3,-3)
                                                  //| (-1,-1)
                                                  //| (0,0)
                                                  //| (4,-2)
                                                  //| (2,-1)
                                                  //| (4,0)
                                                  //| (2,0)
                                                  //| (1,-2)
                                                  //| (3,0)
                                                  //| (0,-2)
                                                  //| (1,-1)
                                                  //| (-1,0)
                                                  //| (4,-1)
                                                  //| (3,-4)
                                                  //| (1,0)
                                                  //| (-1,-2)
                                                  //| (3,-2)
                                                  //| (0,-1)
                                                  //| (3,-1)
                                                  //| (2,-2)
                                                  //| Solution
                                                  //| (-1,-1)
                                                  //| (-2,0)
                                                  //| (0,0)
                                                  //| (-3,-3)
                                                  //| (-2,-4)
                                                  //| (-2,1)
                                                  //| (-2,-3)
                                                  //| (0,-2)
                                                  //| (-1,-3)
                                                  //| (-1,0)
                                                  //| (-2,-1)
                                                  //| (-1,-4)
                                                  //| (-4,-3)
                                                  //| (0,1)
                                                  //| (-1,1)
                                                  //| (-2,-2)
                                                  //| (0,-3)
                                                  //| (0,-4)
                                                  //| (-1,-2)
                                                  //| (0,-1)
                                                  //| Solution
                                                  //| (-3,2)
                                                  //| (-3,4)
                                                  //| (-2,0)
                                                  //| (-3,0)
                                                  //| (0,2)
                                                  //| (0,0)
                                                  //| (-3,3)
                                                  //| (-3,1)
                                                  //| (-2,1)
                                                  //| (-4,2)
                                                  //| (1,1)
                                                  //| (-1,0)
                                                  //| (-4,1)
                                                  //| (-1,2)
                                                  //| (0,1)
                                                  //| (-2,2)
                                                  //| (-1,1)
                                                  //| (1,2)
                                                  //| (-4,0)
                                                  //| (1,0)
                                                  //| Solution
                                                  //| (0,2)
                                                  //| (0,0)
                                                  //| (2,-1)
                                                  //| (2,0)
                                                  //| (0,3)
                                                  //| (1,1)
                                                  //| (1,4)
                                                  //| (0,4)
                                                  //| (1,-1)
                                                  //| (1,3)
                                                  //| (2,2)
                                                  //| (2,4)
                                                  //| (0,1)
                                                  //| (3,3)
                                                  //| (2,3)
                                                  //| (1,2)
                                                  //| (2,1)
                                                  //| (4,3)
                                                  //| (1,0)
                                                  //| (0,-1)
  }
 //}
}