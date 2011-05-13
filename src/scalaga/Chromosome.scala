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

final case class Chromosome(val x: Int, val y: Int) {
  val xLim = 20
  val yLim = 5
  val rounders : IndexedSeq[Double => Int] = IndexedSeq(
      _.round.toInt,
      _.floor.toInt,
      _.ceil.toInt
  )
  
  def mutate = {
    // look around in the area of the current chromosome
    Chromosome((x + (2 * Random.nextGaussian).toInt) % xLim, (y + (2 * Random.nextGaussian).toInt) % yLim)
  }
  
  def mate(other: Chromosome) = {
    // for now an acceptable mate'ing seems to be taking the closest integer point in the middle
    // more advanced methods could later be tried
    val roundMethod = rounders(Random.nextInt(rounders.length))
    
    Chromosome(roundMethod((x + other.x) / 2.0), roundMethod((y + other.y) / 2.0))
  }
  
  def distance(ox: Int, oy: Int) = {
    // vector difference, note that the order is not really important for the distance metric
    val (xdiff, ydiff) = (x - ox, y - oy)
    
    // euclides distance
    math.sqrt(xdiff * xdiff + ydiff * ydiff)
  }
  
  lazy val fitness = {
    // here we have the majority of the problem specific information
    
    // calculate distance  of unit to every other position
    // TODO: insert dynamic way to specify the distance function
    var totalDistance = 0.0
    
    for (i <- 0 until xLim; if i != x; j <- 0 until yLim; if j != y) {
      totalDistance += distance(i, j)
    }
    
    totalDistance
  }
}

/**
 * A factory object for constructing new [[net.auxesia.Chromosome]] instances.
 */
object Chromosome {
  def getRandom = Chromosome(Random.nextInt(10), Random.nextInt(5))
}