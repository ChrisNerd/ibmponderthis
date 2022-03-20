object ponder {

  val nTrials = 10000000                          //> nTrials  : Int = 10000000

  class bullet {
    var t = -1: Int
    var collideTime = -1: Double
    var speed = scala.util.Random.nextDouble(): Double
    var collideWithNext = false: Boolean
  }

  def calculateCollideTime(b1: bullet, b2: bullet): Double =
    {
      (b1.speed * b1.t - b2.speed * b2.t) / (b1.speed - b2.speed)
    }                                             //> calculateCollideTime: (b1: ponder.bullet, b2: ponder.bullet)Double

  // Find the index of the next collision, or -1 if no collisions left
  def findMin(b: List[bullet]): Int =
    {
      var mini = -1
      var min = Double.MaxValue
      for (i <- 0 until b.length - 1) // exclude looking at the last bullet
        if (b(i).collideWithNext && b(i).collideTime < min) {
          mini = i
          min = b(i).collideTime
        }
      mini
    }                                             //> findMin: (b: List[ponder.bullet])Int

  // doAllBulletsAnnihilate is a recursive function that takes a list of bullets and simulates all the collisions.
  // It returns true if no bullets remain and false otherwise
  def doAllBulletsAnnihilate(b: List[bullet]): Boolean = {
    if (b isEmpty)
      true
    else {
      val i = findMin(b)
      if (i == -1) false // No collisions left
      else {
        // With items i and i+1 annihilated, update the collision between item at i-1 and i+2
        if (i != 0 && i != b.length - 2) {
          b(i - 1).collideWithNext = b(i - 1).speed < b(i + 2).speed
          if (b(i - 1).collideWithNext)
            b(i - 1).collideTime = calculateCollideTime(b(i - 1), b(i + 2))
        }
        // remove the 2 annihilated bullets from the list and recurse
        doAllBulletsAnnihilate((b take i) ::: (b drop (i + 2)))
      }
    }
  }                                               //> doAllBulletsAnnihilate: (b: List[ponder.bullet])Boolean

  def trial(N: Int): Boolean =
    {
      var b: List[bullet] = Nil
      for (i <- 0 to N - 1) {
        val bul = new bullet
        // A bullet is fired every second
        bul.t = i
        b = b ::: List(bul)
      }

      for (i <- 0 to N - 2) {
        b(i).collideWithNext = b(i).speed < b(i + 1).speed
        if (b(i).collideWithNext)
          b(i).collideTime = calculateCollideTime(b(i), b(i + 1))
      }

      doAllBulletsAnnihilate(b)
    }                                             //> trial: (N: Int)Boolean

  for (N <- 2 to 20 by 2) {
    var count = 0;
    for (i <- 1 to nTrials) {
      if (trial(N))
        count+=1
    }
    println(s"N=$N, probability=0.$count")
  }                                               //> N=2, probability=0.4997682
                                                  //| N=4, probability=0.3747262
                                                  //| N=6, probability=0.3125067
                                                  //| N=8, probability=0.2734030
                                                  //| N=10, probability=0.2460309
                                                  //| N=12, probability=0.2257556
                                                  //| N=14, probability=0.2095387
                                                  //| N=16, probability=0.1963739
                                                  //| N=18, probability=0.1852485
                                                  //| N=20, probability=0.1761309
}