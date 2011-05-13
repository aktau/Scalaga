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

/**
 * Object driver for the genetic algorithm "Hello, world!" simulation.
 * 
 * @author John Svazic and Nicolas Hillegeer
 */
object GAHelloWorld {

  /**
   * Main driver function for the simulation.
   * 
   * @param args Command-line arguments (ignored).
   */
  def main(args: Array[String]): Unit = {
	val startTime = System.currentTimeMillis
    // The size of the simulation population
    val populationSize = 30 //2048

    // The maximum number of generations for the simulation.
    val maxGenerations = 40 //16384

    // The probability of crossover for any member of the population,
    // where 0.0 <= crossoverRatio <= 1.0
    val crossoverRatio = 0.8f

    // The portion of the population that will be retained without change
    // between evolutions, where 0.0 <= elitismRatio < 1.0
    val elitismRatio = 0.1f

    // The probability of mutation for any member of the population,
    // where 0.0 <= mutationRatio <= 1.0
    val mutationRatio = 0.03f

    // Create the initial population
    val pop = Population(populationSize, crossoverRatio, elitismRatio, mutationRatio)

    // Start evolving the population, stopping when the maximum number of
    // generations is reached, or when we find a solution.
    
    printObjFun

    var generation = 1;
    while (generation <= maxGenerations && pop.population(0).fitness != 0) {
      println("Generation " + generation + ": " + pop.population(0) + " (fitness = " + pop.population(0).fitness + ")")
      println(pop)
      pop.evolve
      generation += 1
    }
    val endTime = System.currentTimeMillis
    
    println("Generation " + generation + ": " + pop.population(0).fitness)
    println("Total execution time: " + (endTime - startTime) + "ms")
  }
  
  /**
   * For debugging purposes
   */
  def printObjFun {
    val matrix: Array[Array[Double]] = Array.fill(20,5)(0.0)
    var bestChrom = Chromosome(0,0)
    
    for (i <- 0 until 20; j <- 0 until 5) {
      val chrom = Chromosome(i,j)
      
      matrix(i)(j) = chrom.fitness
      
      bestChrom = if (bestChrom.fitness > chrom.fitness) chrom else bestChrom
    }
    
    //for (i <- 0 until matrix.length) printf("%d : [min: %f] %s\n", i, matrix(i).min, matrix(i).mkString("[", " ", "]"))
    
    printf("BEST CHROMOSOME VALUE: %s (%f)\n", bestChrom, bestChrom.fitness)
  }
}