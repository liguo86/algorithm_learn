package com.liguo86.algorithm.genetic.tsp

case class Individual(val chromosome: Array[Int], var fitness: Double = -1D) {

  def this(chromosomeLength: Int) = this((0 to chromosomeLength - 1).map(r => r).toArray)

  override def toString: String = chromosome.mkString("")
}