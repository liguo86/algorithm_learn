package com.liguo86.algorithm.genetic.robot

case class Individual(val chromosome: Array[Int], var fitness: Double = -1D) {

  def this(chromosomeLength: Int) = this((0 to chromosomeLength - 1).map(r => if (Math.random() > 0.5) 1 else 0).toArray)

  override def toString: String = chromosome.mkString("")
}
