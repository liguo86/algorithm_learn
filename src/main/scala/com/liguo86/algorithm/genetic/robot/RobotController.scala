package com.liguo86.algorithm.genetic.robot

object RobotController {
  val maxGenerations = 1000

  def main(args: Array[String]): Unit = {
    /**
      * As a reminder:
      * 0 = Empty
      * 1 = Wall
      * 2 = Starting position
      * 3 = Route
      * 4 = Goal position
      */
    val maze = new Maze(Array[Array[Int]](
      Array(0, 0, 0, 0, 1, 0, 1, 3, 2),
      Array(1, 0, 1, 1, 1, 0, 1, 3, 1),
      Array(1, 0, 0, 1, 3, 3, 3, 3, 1),
      Array(3, 3, 3, 1, 3, 1, 1, 0, 1),
      Array(3, 1, 3, 3, 3, 1, 1, 0, 0),
      Array(3, 3, 1, 1, 1, 1, 0, 1, 1),
      Array(1, 3, 0, 1, 3, 3, 3, 3, 3),
      Array(0, 3, 1, 1, 3, 1, 0, 1, 3),
      Array(1, 3, 3, 3, 3, 1, 1, 1, 4)))

    // Create genetic algorithm
    val ga = new GeneticAlgorithm(200, 0.05, 0.9, 2, 10)
    var population = ga.initPopulation(128)
    ga.evalPopulation(population, maze)
    // Keep track of current generation
    var generation = 1
    // Start evolution loop
    while ( {
      ga.isTerminationConditionMet(generation, maxGenerations) == false
    }) { // Print fittest individual from population
      val fittest = population.getFittest(0)
      System.out.println("G" + generation + " Best solution (" + fittest.fitness + "): " + fittest.toString)
      // Apply crossover
      population = ga.crossoverPopulation(population)
      // Apply mutation
      population = ga.mutatePopulation(population)
      // Evaluate population
      ga.evalPopulation(population, maze)
      // Increment the current generation
      generation += 1
    }

    System.out.println("Stopped after " + maxGenerations + " generations.")
    val fittest = population.getFittest(0)
    System.out.println("Best solution (" + fittest.fitness + "): " + fittest.toString)
  }
}
