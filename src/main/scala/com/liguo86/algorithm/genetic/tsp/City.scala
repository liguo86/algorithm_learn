package com.liguo86.algorithm.genetic.tsp

case class City(x: Int, y: Int) {
  def distanceFrom(city: City): Double = {
    Math.sqrt(Math.pow(city.x - this.x, 2) + Math.pow(city.y - this.y, 2))
  }
}
