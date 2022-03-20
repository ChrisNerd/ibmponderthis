//val sentence = "a toast to satan wanton ottawa saw santa"
import java.util.Collection

object ponder {

  /*
val sentence = "send more"
val solutionWord = "money"
*/

/*
  val sentence = "a toast to satan"
  val solutionWord = "watson"
*/

//val sentence = "rattrap tiara qatar"
//cut -f 1 -d '/' en_GB-oed.dic | grep '^[phillipPHILLIPQATARqatar]*$'
 // val sentence = "haiti tahiti qatar trip"
//val solutionWord = "phillip"
  //val solutionWord = "phil"
  
  val sentence = "phil art at"                    //> sentence  : String = phil art at
  val solutionWord = "qatar"                      //> solutionWord  : String = qatar
  //val sentence = "wow town ottawa" // no solutions
  // val sentence = "ant at nasa sat on snow"
  val wordsSubsets = sentence.split(" ").toSet.subsets
                                                  //> wordsSubsets  : Iterator[scala.collection.immutable.Set[String]] = non-empty
                                                  //|  iterator

  // The map that defines the relationship from Charactors to Digits
  val m = scala.collection.mutable.Map[Char, Int]()
                                                  //> m  : scala.collection.mutable.Map[Char,Int] = Map()

  // Convert a single charactor, say w, to its corresponding digit, say 1.
  def charToDigit(c: Char): Int = m getOrElse (c, 0)
                                                  //> charToDigit: (c: Char)Int

  // Converts an entire word, say "watson", to an integer, say 123456, based on the map m.
  def wordToNum(word: String): Int = {
    val wordNumList = (word zipWithIndex) map (x => charToDigit(x._1) * math.pow(10, word.length - x._2 - 1))
    wordNumList.sum.toInt
  }                                               //> wordToNum: (word: String)Int

  // Recursively create a list of length 3^n of n elements each of the 3 characters -,+ and *.
  // alloperators(2) returns
  // --
  // -+
  // -*
  // +-
  // ...
  // **
  def alloperators(n: Int): List[List[Char]] = n match {
    case 1 => List(List('-'), List('+'), List('*'))
    case _ =>
      for {
        firstOperator <- alloperators(1)
        allbutfirstOperators <- alloperators(n - 1)
      } yield firstOperator ::: allbutfirstOperators
  }                                               //> alloperators: (n: Int)List[List[Char]]

  // Parses and evaluates a simple mathematical expression
  // WARNING!: operatorsList must not start with a leading *.  It doesn't make mathematical sense anyway.
  // calcTree (1,2,3,4), (-,+,*,*))
  // = -1+2*3*4
  // = calcTree (1,6,8), (-,+,*))
  // = calcTree (1,48), (-,+,))
  // = (-1, 48).sum
  def calcTree(numbersList: List[Int], operatorsList: List[Char]): Int = {
    // order of operations: take care of the multiplications first
    (operatorsList zipWithIndex).find(_._1 == '*') match {
      case Some((_, operate_INDEX)) =>
        calcTree((numbersList take (operate_INDEX - 1)) ::: List(numbersList(operate_INDEX - 1) * numbersList(operate_INDEX)) :::
          (numbersList drop (operate_INDEX + 1)),
          (operatorsList take (operate_INDEX)) ::: (operatorsList drop (operate_INDEX + 1)))
      case None => // no multiplications
        val numbersListWithSign = (numbersList zip operatorsList) map (x => {
          if (x._2 == '-')
            -1 * x._1
          else
            x._1
        })
        numbersListWithSign.sum
    }
  }                                               //> calcTree: (numbersList: List[Int], operatorsList: List[Char])Int-

  // Try all distinct subsets of our sentence
  // Note that this eliminates duplicate words
  // "to toast to" would be reduced to "to toast"
  for {
    wordsSubset <- wordsSubsets
    if (wordsSubset.size > 0) // first letter check fails with the empty list
  } {
    val wordsSubsetList = wordsSubset.toList
    // letters countains all the unique letters used and will be used to index into the candidateMap
    val letters = (solutionWord.toSet ++ wordsSubset.flatten).toList zipWithIndex

    // Create a list of all potential mappings from char to digit for the word "watson"
    // Its size will be 10 P 6 = 10!/4!=151,200, which isn't too bad for brute forcing.
    // But elimination of leading zeros
    // allCandidates =
    // 012345
    // 012354
    // ...
    // 987645
    // 987654
    // So 102345 would be eliminated since y(1) == 0 if letters(1) is a leading letter
    val allCandidateMapsSet = for {
      x <- (0 to 9) combinations (letters.size)
      y <- x.permutations
      if !((wordsSubset + solutionWord) map (word => word(0)) exists (firstletter =>

        //        (solutionWord zipWithIndex).find(_._1 == firstletter) match {
        //          case Some((_, indexInSolutionWord)) =>
        //            y(indexInSolutionWord) == 0
        letters find (_._1 == firstletter) match {
          case Some((_, indexInM)) =>
            y(indexInM) == 0
        }))
      /*       if (firstletter == 'w')
          y(0) == 0
        else if (firstletter == 'a')
          y(1) == 0
        else if (firstletter == 't')
          y(2) == 0
        else if (firstletter == 's')
          y(3) == 0
        else if (firstletter == 'o')
          y(4) == 0
        else //(firstletter == 'n')
          y(5) == 0))
   */
    } yield y.toList
    // Convert from iterator to list so we can reuse it.
    val allCandidateMaps = allCandidateMapsSet.toList
    // Cycle through the operators one at a time first
    for {
      operators <- alloperators(wordsSubsetList.size) filter (op => op(0) != '*') // first operator must not be *
    } yield {
      // Evaluate all candidate mappings for this particular wordSubset and operators -> see if there's only 1 solution.
      val allEquationsEvaluated =
        for {
          candidateMap <- allCandidateMaps
        } yield {
          for ((letter, index) <- letters)
            m.update(letter, candidateMap(index))
          /*         m.update('w', candidateMap(0))
            m.update('a', candidateMap(1))
            m.update('t', candidateMap(2))
            m.update('s', candidateMap(3))
            m.update('o', candidateMap(4))
            m.update('n', candidateMap(5))
*/
          val wordNums = wordsSubsetList map wordToNum
          (calcTree(wordNums, operators), wordToNum(solutionWord), wordsSubsetList, wordNums, candidateMap, operators)
        }

      val solutions = allEquationsEvaluated.toList filter (f => f._1 == f._2)
      if (solutions.length == 1)
        println(solutions)
    }
  }

}

// List((357914,357914,List(a, toast, to),List(5, 71597, 71),List(3, 5, 7, 9, 1 , 4),List(+, *, -)))
                                                  
// a * toast - to == watson == 357914


// List((10652,10652,Set((send,9567), (more,1085)),List(5, 9, 6, 2, 1, 8, 0, 7),List(+, +)))-


/*
grep '^[watson]*$' cracklib-small

a
an
anna
ant
ants
as
ass
assonant
at
nan
nasa
no
non
noon
noons
not
now
oats
on
onto
ottawa
own
owns
san
sans
santa
sat
satan
saw
saws
snow
snows
so
son
sons
soon
soot
sow
stanton
stow
swan
swans
swanson
swat
swoon
tan
tao
tattoo
tattoos
to
toast
toasts
ton
tons
too
toot
toss
tot
tow
town
towns
two
twos
want
wanton
wants
was
watson
watt
watts
won
woo
woos
wow
*/

/*val goodWords=List("a",
ant
ants
as
ass
at
nan
nasa
no
non
noon
not
now
oats
on
onto
ottawa
own
owns
santa
sat
satan
saw
saws
snow
snows
so
son
sons
soon
soot
swan
swans
swat
swoon
tan
tao
tattoo
tattoos
to
toast
toasts
ton
tons
too
toot
toss
tot
tow
town
towns
two
twos
want
wanton
wants
was
watson
watt
watts
won
wow")
*/