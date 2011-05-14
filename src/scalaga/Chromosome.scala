/*
 * The MIT License
 * 
 * Copyright (c) 2011 John Svazic
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package scalaga

import scala.util.Random

object mathExtra {
  def clamp(n: Int, low: Int, high: Int) = {
    if (n > high) high
    else if (n < low) low
    else n
  }
}

import mathExtra._

final case class Chromosome(val x: Int, val y: Int) {
  val xLim = 10
  val yLim = 5
  val rounders : IndexedSeq[Double => Int] = IndexedSeq(
      _.round.toInt,
      _.floor.toInt,
      _.ceil.toInt
  )
  
  def mutate = {
    // look around in the area of the current chromosome
    Chromosome(
        clamp(x + (2 * Random.nextGaussian).toInt, 0, xLim - 1), 
        clamp(y + (2 * Random.nextGaussian).toInt, 0, yLim - 1)
    )
  }
  
  def mate(other: Chromosome) = {
    // for now an acceptable mate'ing seems to be taking the closest integer point in the middle
    // more advanced methods could later be tried
    val roundMethod = rounders(Random.nextInt(rounders.length))
    
    Chromosome(roundMethod((x + other.x) / 2.0), roundMethod((y + other.y) / 2.0))
  }
  
  lazy val fitness = {
    // here we have the majority of the problem specific information
    
    // calculate distance  of unit to every other position
    // TODO: insert dynamic way to specify the distance function
    var totalDistance = 0.0
    
    val riverHeight = 2
    val bridge1 = (2.5,2.5)
    val bridge2 = (7.5,2.5)
    
    for (i <- 0 until xLim; if i != x; j <- 0 until yLim; if j != y) {
      if ((j > riverHeight && y > riverHeight) || (j <= riverHeight && y <= riverHeight)) {
        // same side
        totalDistance += Chromosome.euclidDistance(x, y, i, j)
      }
      else {
        totalDistance += math.min(
            Chromosome.euclidDistance(x, y, bridge1._1, bridge1._2) + Chromosome.euclidDistance(bridge1._1, bridge1._2, i, j),
            Chromosome.euclidDistance(x, y, bridge2._1, bridge2._2) + Chromosome.euclidDistance(bridge2._1, bridge2._2, i, j)
        )
      }
    }
    
    totalDistance
  }
}

/**
 * A factory object for constructing new [[net.auxesia.Chromosome]] instances.
 */
object Chromosome {
  def getRandom = Chromosome(Random.nextInt(10), Random.nextInt(5))
  
  def euclidDistance(x: Double, y: Double, x2: Double, y2: Double) = {
    val (xdiff, ydiff) = (x - x2, y - y2)
    
    // euclides distance
    math.sqrt(xdiff * xdiff + ydiff * ydiff)
  }
}