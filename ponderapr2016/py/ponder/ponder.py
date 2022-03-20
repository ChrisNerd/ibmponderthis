import matplotlib.pyplot as plt
import math
allCandidates = [(x1, x2, 300-x1-x2) for x1 in range(1,101) for x2 in range(x1, math.floor((300-x1)/2) + 1)]
MAXEXPERIMENTS=16
MAXCATS=6

def maxDistinguishableCases(d, lefts):
    if lefts == 0:
        return 1
    if d == MAXEXPERIMENTS:
        return 1
    return maxDistinguishableCases(d + 1, lefts - 1) + maxDistinguishableCases(d + 1, lefts)
  
print(maxDistinguishableCases(14,4))
def p(n)(x):
  x(0) <= n && n <= x(1) || x(2) <= n


def outputIfSolvable(candidates, experimentNumber, catsLeft, printB, history = Vector()):
  if (candidates.size == 1):
    if (printB):
      print("After " + (experimentNumber - 1) + " experiments with " + catsLeft + " cats left, we conclude that the roots are " + candidates.head + "\nThe path we took to get here is")
      for h in history:
	print(h)
    true
  elif (experimentNumber <= MAXEXPERIMENTS and catsLeft > 0):
    idealRatio = 1.0 * maxDistinguishableCases(experimentNumber + 1, catsLeft - 1) / maxDistinguishableCases(experimentNumber, catsLeft)
    maxDistinguishableCasesLeft = maxDistinguishableCases(experimentNumber + 1, catsLeft - 1)
    maxDistinguishableCasesRight = maxDistinguishableCases(experimentNumber + 1, catsLeft)
  sorted(range(1,301), key=lambda n:
    failedNodes = filter(p(n),candidates)
    ratioError = failedNodes.size.toDouble / candidates.size - idealRatio
    Math.abs(ratioError)
  )     find (k =>
        /*
        Find, if possible, a floor k such that after we run the test at this floor, futher tests will all yield unique solutions.
        */
          {
          // "Failed" as in the cat died, (p(k)>=0), leaving further experiments with catsLeft-1.
            val (failedCandidates, passedCandidates) = candidates partition p(k)
            (
              failedCandidates.size > 0 &&  // The experiment is useless if all the tests either pass or fail
              passedCandidates.size > 0 &&
              failedCandidates.size <= maxDistinguishableCasesLeft &&  // We know that future experiments can never distinguish that many cases
              passedCandidates.size <= maxDistinguishableCasesRight &&
              outputIfSolvable(failedCandidates, experimentNumber + 1, catsLeft - 1, false) &&
              outputIfSolvable(passedCandidates, experimentNumber + 1, catsLeft, false))
          }) match {
          case Some(k) =>
            {
              if (printB) {
                println(experimentNumber, catsLeft, k)
                val (failedCandidates, passedCandidates) = candidates partition p(k)
                // unfortunately, we have to do the reusion and solve again, but we'll pass in the history this time.
                outputIfSolvable(failedCandidates, experimentNumber + 1, catsLeft - 1, true, history :+ (k, false))
                outputIfSolvable(passedCandidates, experimentNumber + 1, catsLeft,     true, history :+ (k,true))
              }
              true
            }
          case None => false // No possible  solution, pass it up a level
        }

      }
      else false // More than 1 candidate,and over our expermiment or cat budget
    }