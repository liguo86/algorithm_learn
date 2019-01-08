package com.liguo86.algorithm.genetic.tsp

object TSP {
  val maxGenerations = 10000
  def main(args: Array[String]): Unit = {
    // Create cities
    val numCities = 100

    // Loop to create random cities
    val cities= (0 to numCities-1).map{_ => // Generate x,y position
      val xPos = (100 * Math.random).toInt
      val yPos = (100 * Math.random).toInt
      // Add city
       new City(xPos, yPos)
    }.toArray

    // Initial GA
    val ga = new GeneticAlgorithm(100, 0.001, 0.9, 2, 5)

    // Initialize population
    var population = ga.initPopulation(cities.length)

    // Evaluate population
    ga.evalPopulation(population, cities)

    val startRoute = new Route(population.getFittest(0), cities)
    println("Start Distance: " + startRoute.getDistance)

    // Keep track of current generation
    var generation = 1
    // Start evolution loop
    while ( {
      ga.isTerminationConditionMet(generation, maxGenerations) == false
    }) { // Print fittest individual from population
      val route = new Route(population.getFittest(0), cities)
      println("G" + generation + " Best distance: " + route.getDistance)
      // Apply crossover
      population = ga.crossoverPopulation(population)
      // Apply mutation
      population = ga.mutatePopulation(population)
      // Evaluate population
      ga.evalPopulation(population, cities)
      // Increment the current generation
      generation += 1
    }

    println("Stopped after " + maxGenerations + " generations.")
    val route = new Route(population.getFittest(0), cities)
    println("Best distance: " + route.getDistance)
  }
}
