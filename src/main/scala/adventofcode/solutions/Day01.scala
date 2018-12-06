package adventofcode.solutions

import adventofcode._

import scala.collection.mutable

object Day01 extends Day(1) {

  val frequencyChanges = input.map(_.toInt).toSeq

  val partA = frequencyChanges.reduce(_ + _).toString
  solution(partA, A)

  val circularFrequencies = Stream.continually(frequencyChanges).flatten
  private def findDuplicateFrequency(currentFrequency: Int, frequenciesFound: mutable.Set[Int],
                                     frequencies: Stream[Int]): Int = {
    val nextFrequency = currentFrequency + frequencies.head
    if (frequenciesFound.contains(nextFrequency))
    {
      return nextFrequency
    }
    frequenciesFound.add(nextFrequency)
    return findDuplicateFrequency(nextFrequency, frequenciesFound, frequencies.tail)
  }

  val partB = findDuplicateFrequency(0, mutable.Set.empty[Int], circularFrequencies).toString
  solution(partB, B)
}
