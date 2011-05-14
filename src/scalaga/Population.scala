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

import scala.collection.Iterator
import scala.collection.immutable.Vector
import scala.math.round
import scala.util.Random
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ArrayBuffer
import scala.collection.IndexedSeqOptimized

object Shuffle {
  /**
   * The generic version
   */
  def randomSubset[T, CC[X] <: IndexedSeqOptimized[X,CC[X]]](xs: CC[T], n: Int)(implicit bf: CanBuildFrom[CC[T], T, CC[T]]): CC[T] = {
    val buf = new ArrayBuffer[T](n)
    
    val size = xs.length
    val stop = xs.length - 1
    
    var i = 0
    var j = 0
    
    while (j < n && i != stop) {
      if (Random.nextInt(size - i) < n - j) {
        buf += xs(i)
        
        j += 1
      }
      
      i += 1
    }

    bf(xs) ++= buf result
  }
  
  /**
   * The array-only version (because arrays are that goddamn special)
   */
  def arrayRandomSubset[T](xs: Array[T], n: Int) : ArrayBuffer[T] = {
    val buf = new ArrayBuffer[T](n)
    
    val size = xs.length
    val stop = xs.length - 1
    
    var i = 0
    var j = 0
    
    while (j < n && i != stop) {
      if (Random.nextInt(size - i) < n - j) {
        buf += xs(i)
        
        j += 1
      }
      
      i += 1
    }
    
    buf
  }
}

abstract class AbstractPopulation(var _population: Array[Chromosome]) {
  protected def selectParents: (Chromosome, Chromosome)
}

trait HoldoutTournamentSelection extends AbstractPopulation {
  val tournamentSize = 3
  
  /**
   * This tournament selection operator makes sure both parents are distinct. In small populations,
   * this could be pretty important to maintain genetic diversity without have to amplify the mutation rate too much
   */
  override protected def selectParents: (Chromosome, Chromosome) = {
    val pop = (new ArrayBuffer() ++ _population)
    
    val first = Shuffle.randomSubset(pop, tournamentSize).sortWith(_.fitness < _.fitness).head
    
    pop -= first
    
    val second = Shuffle.randomSubset(pop, tournamentSize).sortWith(_.fitness < _.fitness).head
    
    (first, second)
  }
}

class Population(_pop: Array[Chromosome], val crossover: Float, val elitism: Float, val mutation: Float) extends AbstractPopulation(_pop) {
  /**
   * A public accessor for the underlying vector of [[net.auxesia.Chromosome]]
   * objects.
   */
  def population = _population
  
  /**
   * Default implementation is a tournament implementation
   */
  override protected def selectParents: (Chromosome, Chromosome) = {
    val tournamentSize = 3
    
    def makeTournament : Chromosome = {
      Shuffle.arrayRandomSubset(_population, tournamentSize).sortWith(_.fitness < _.fitness).head
    }
    
    (makeTournament, makeTournament)
  }

  /**
   * Method used to evolve a new generation for the population.  This method
   * modifies the internal population represented by this class.
   */
  def evolve = {
    // Create a buffer for the new generation
    val elitismCount = math.ceil(_population.length * elitism)

    def randomMutate(ch: Chromosome): Chromosome = {
      if (Random.nextFloat <= mutation) ch.mutate else ch
    }

    var idx = 0
    val buffer = new Array[Chromosome](_population.length)

    while (idx < buffer.length) {
      if (idx < elitismCount) {
        buffer(idx) = _population(idx)
        
        printf("Elitism:\t%s\n", buffer(idx))
      } 
      else if (Random.nextFloat() <= crossover) {
        // Select the parents and mate to get their children
        val parents = selectParents
        val child = parents._1 mate parents._2
        
        buffer(idx) = randomMutate(child)
        
        printf("Crossover:\t%s and %s => %s (without mutation: %s)\n", parents._1, parents._2, buffer(idx), child)
      } 
      else {
        buffer(idx) = randomMutate(_population(idx))
        
        printf("Mutation:\t%s => %s\n", _population(idx), buffer(idx))
      }

      idx += 1
    }

    _population = buffer.sortWith((s, t) => s.fitness < t.fitness)
  }

  override def toString = {
    var result = ""

    for (chromosome <- _population) {
      result += chromosome + " (fitness = " + chromosome.fitness + ")\n"
    }

    result
  }
}

/**
 * Factory for [[net.auxesia.Population]] instances.
 */
object Population {
  /**
   * Create a [[net.auxesia.Population]] with a given size, crossover ratio, elitism ratio
   * and mutation ratio.
   *
   * @param size The size of the population.
   * @param crossover The crossover ratio for the population.
   * @param elitism The elitism ratio for the population.
   * @param mutation The mutation ratio for the population.
   *
   * @return A new [[net.auxesia.Population]] instance with the defined
   * parameters and an initialized set of [[net.auxesia.Chromosome]] objects
   * representing the population.
   */
  def apply(size: Int, crossover: Float, elitism: Float, mutation: Float) = {
    new Population(generateInitialPopulation(size), crossover, elitism, mutation)
  }

  /**
   * Helper method used to generate an initial population of random
   * [[net.auxesia.Chromosome]] objects for a given population size.
   *
   * @param size The size of the population.
   *
   * @return A [[scala.collection.immutable.List]] of the defined size
   * populated with random [[net.auxesia.Chromosome]] objects.
   */
  def generateInitialPopulation(size: Int): Array[Chromosome] = {
    new Array[Chromosome](size).map(i => Chromosome.getRandom).sortWith(
      (s, t) => s.fitness < t.fitness)
  }
}