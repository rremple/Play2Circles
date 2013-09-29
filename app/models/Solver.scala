package models

object Solver {
  val recursionLimit: Int = 1000

  /*
   *  Generic, when the "finished" function needs the last two terms (e.g., looking for general convergence)
   */
  def iterateUntil[A, B](finished: (A, A) => Boolean)(nextTerm: B => B)(extractVal: B => A)(start: B): Stream[A] = {
    def streamGenerator(previous: B): Stream[A] = extractVal(previous) #:: {
      val next = nextTerm(previous)
      if (finished(extractVal(previous), extractVal(next))) Stream.empty else streamGenerator(next)
    }
    streamGenerator(start)
  }

  /*
   * Generic, when the "finished" function only needs the last term (e.g., looking for convergence to a known value)
   */
  def iterateUntil[A, B](finished: (A) => Boolean)(nextTerm: B => B)(extractVal: B => A)(start: B): Stream[A] = {
    def streamGenerator(previous: B): Stream[A] = extractVal(previous) #:: {
      val next = nextTerm(previous)
      if (finished(extractVal(next))) Stream.empty else streamGenerator(next)
    }
    streamGenerator(start)
  }

  def getLast[A](stream: Stream[A], messageTag: String) = {
    val list = stream.take(recursionLimit)
    if (list.size < recursionLimit) list.last else sys.error(messageTag + ": recursion too deep")
  }

  /*
   * Works for common cases: continuous increasing/decreasing functions of doubles returning doubles
   */
  def solveForDouble(b: Double => Boolean)(minBoundry: Double, maxBoundry: Double, targetVal: Double)(f: Double => Double) = {
    case class DoubleTerm(count: Int, minVal: Double, maxVal: Double, guess: Double)
    def doubleTermMethodNext(previous: DoubleTerm) = {
      val count = previous.count + 1
      val guessVal = (previous.minVal + previous.maxVal) / 2
      //println(s"count: $count, previous: $previous")
      val guessError = f(guessVal) - targetVal
      if (b(guessError)) DoubleTerm(count + 1, guessVal, previous.maxVal, guessVal)
      else DoubleTerm(count + 1, previous.minVal, guessVal, guessVal)
    }
    Solver.getLast(Solver.iterateUntil { (guess: Double) =>
      Scalar.veryClose(f(guess), targetVal)
    }(doubleTermMethodNext)(_.guess)(DoubleTerm(0, minBoundry, maxBoundry, 0)), "continuous decreasing function")
  }

  def solveForDecreasing = solveForDouble(0 <) _
  def solveForIncreasing = solveForDouble(0 >) _
}