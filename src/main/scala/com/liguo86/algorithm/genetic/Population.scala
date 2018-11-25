package com.liguo86.algorithm.genetic

import java.util.Random

case class Population(val population: Array[Individual], var populationFitness: Double = -1D) {
  def this(populationSize: Int) = this(new Array[Individual](populationSize))

  def this(populationSize: Int, chromosomeLength: Int) = this((0 to populationSize - 1).
    map(r => new Individual(chromosomeLength)).toArray)

  def getFittest(offset: Int): Individual = {
    this.population.sortBy(-_.fitness).apply(offset)
  }

  def setIndividual(offset: Int, individual: Individual): Individual = {
    this.population(offset) = individual
    individual
  }

  def shuffle(): Unit = {
    val rnd = new Random
    (this.population.length - 1 to 1).by(-1).foreach { i =>
      val index = rnd.nextInt(i + 1)
      val a = this.population(index)
      this.population(index) = this.population(i)
      this.population(i) = a
    }
  }
}
