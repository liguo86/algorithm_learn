package com.liguo86.algorithm.genetic

object AllOnesGA {
  def main(args: Array[String]): Unit = {
    // Create GA object
    val ga = new GeneticAlgorithm(100, 0.001, 0.95, 2)

    // Initialize population
    var population = ga.initPopulation(50)

    // Evaluate population
    ga.evalPopulation(population)

    // Keep track of current generation
    var generation = 1

    /**
      * Start the evolution loop
      *
      * Every genetic algorithm problem has different criteria for finishing.
      * In this case, we know what a perfect solution looks like (we don't
      * always!), so our isTerminationConditionMet method is very
      * straightforward: if there's a member of the population whose
      * chromosome is all ones, we're done!
      */
    while ( {
      ga.isTerminationConditionMet(population) == false
    }) { // Print fittest individual from population
      System.out.println("Best solution: " + population.getFittest(0).toString)
      // Apply crossover
      population = ga.crossoverPopulation(population)
      // Apply mutation
      population = ga.mutatePopulation(population)
      // Evaluate population
      ga.evalPopulation(population)
      // Increment the current generation
      generation += 1
    }

    /**
      * We're out of the loop now, which means we have a perfect solution on
      * our hands. Let's print it out to confirm that it is actually all
      * ones, as promised.
      */
    System.out.println("Found solution in " + generation + " generations")
    System.out.println("Best solution: " + population.getFittest(0).toString)
  }
}
