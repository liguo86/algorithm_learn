package com.liguo86.algorithm.genetic.tsp

case class Route(var route: Array[City], var distance: Double = 0d) {

  def this(individual: Individual, cities: Array[City]) = {
    this(cities)
    route = (0 to individual.chromosome.length - 1).map { i =>
      cities(individual.chromosome(i))
    }.toArray
  }

  def getDistance() = {
    if (this.distance > 0) {
      this.distance
    } else {
      val total = (0 to this.route.length - 2).map { i =>
        route(i).distanceFrom(route(i + 1))
      }.sum
      total + route(route.length - 1).distanceFrom(route(0))
    }
  }
}
