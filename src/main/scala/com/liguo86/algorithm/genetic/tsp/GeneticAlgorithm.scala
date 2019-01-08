package com.liguo86.algorithm.genetic.tsp

import scala.util.control.Breaks

/**
  *
  * @param populationSize 人口规模
  * @param mutationRate   变异率 （范围0-1，一般比较小，如0.1或者更小）
  * @param crossoverRate  交换率 （范围0-1）
  * @param elitismCount   精英计数 （精英是不需要进行交换和变异的直接进入下一代）
  * @param tournamentSize 锦标赛规模
  */
class GeneticAlgorithm(val populationSize: Int, val mutationRate: Double, val crossoverRate: Double, val elitismCount: Int, val tournamentSize: Int) {
  /**
    * 根据染色体长度初始化人口规模
    *
    * @param chromosomeLength
    * @return
    */
  def initPopulation(chromosomeLength: Int): Population = {
    new Population(this.populationSize, chromosomeLength)
  }

  /**
    * 计算个体的适应度（健康度）
    *
    * @param individual
    * @param cities
    * @return
    */
  def calcFitness(individual: Individual, cities: Array[City]): Double = {
    val route = new Route(individual, cities)
    val fitness = 1 / route.getDistance()
    individual.fitness = fitness
    fitness
  }


  /**
    * 评估整个群体的适应度（健康度）
    *
    * @param population
    * @param cities
    */
  def evalPopulation(population: Population, cities: Array[City]) = {
    val populationFitness = population.population.map(individual => calcFitness(individual, cities)).sum
    population.populationFitness = populationFitness / population.population.length
  }

  /**
    * 检查群体是否满足终止条件
    *
    * @param population
    * @return
    */
  def isTerminationConditionMet(population: Population): Boolean = {
    for (individual <- population.population) {
      if (individual.fitness == 1) return true
    }
    false
  }

  /**
    * 检查群体是否满足终止条件
    *
    * @param population
    * @return
    */
  def isTerminationConditionMet(generationsCount: Int, maxGenerations: Int): Boolean = {
    generationsCount > maxGenerations
  }

  /**
    * 选择一个父亲用于交叉，用锦标赛的方法
    *
    * @param population
    * @return
    */
  def selectParent(population: Population): Individual = {
    population.shuffle()
    new Population((0 to (tournamentSize - 1)).map { i =>
      population.population(i)
    }.toArray).getFittest(0)
  }

  /**
    * 交叉
    *
    * * Parent1: AAAAAAAAAA
    * * Parent2: BBBBBBBBBB
    * * Child  : AABBAABABA
    * *
    * * This version, however, might look like this:
    * *
    * * Parent1: AAAAAAAAAA
    * * Parent2: BBBBBBBBBB
    * * Child  : AAAABBBBBB
    * 这次修改为
    * * * Parent1: AAAAAAAAAA
    * * * Parent2: BBBBBBBBBB
    * * * Child  : AAAABBBBAA
    *
    * @param population
    * @return
    */
  def crossoverPopulation(population: Population): Population = {
    val newPopulation = new Population(population.population.length)
    (0 to population.population.length - 1).foreach { i =>
      val parent1 = population.getFittest(i)
      if (this.crossoverRate > Math.random() && i >= this.elitismCount) {
        val parent2 = selectParent(population)

        val substrPos1 = (Math.random() * (parent1.chromosome.length + 1)).intValue()
        val substrPos2 = (Math.random() * (parent1.chromosome.length + 1)).intValue()

        val startSubstr = Math.min(substrPos1, substrPos2)
        val endSubstr = Math.max(substrPos1, substrPos2)

        val chromosome = (0 to parent1.chromosome.length - 1).map { geneIndex =>
          if (geneIndex >= startSubstr && geneIndex < endSubstr) {
            parent1.chromosome(geneIndex)
          } else {
            -1
          }
        }.toArray[Int]

        val offspring = new Individual(chromosome)
        (0 to parent1.chromosome.length - 1).foreach { geneIndex =>
          var parent2Gene = geneIndex + endSubstr
          if (parent2Gene >= parent2.chromosome.length) parent2Gene -= parent2.chromosome.length

          if (!offspring.chromosome.contains(parent2.chromosome(parent2Gene))) {
            val loop = new Breaks
            loop.breakable {
              for (ii <- 0 to (offspring.chromosome.length - 1)) {
                if (offspring.chromosome(ii) == -1) {
                  offspring.chromosome(ii) = parent2.chromosome(parent2Gene)
                  loop.break()
                }
              }
            }
          }
        }
        newPopulation.setIndividual(i, offspring)
      } else {
        newPopulation.setIndividual(i, parent1)
      }
    }
    newPopulation
  }

  /**
    * 变异种群
    *
    * @param population
    * @return
    */
  def mutatePopulation(population: Population): Population = {
    val individuals = (0 to population.population.length - 1).map { populationIndex =>
      val individual = population.getFittest(populationIndex)
      if (populationIndex > this.elitismCount) {
        val chromosome = (0 to individual.chromosome.length - 1).map { geneIndex =>
          if (this.mutationRate > Math.random()) {
            // Get new gene position
            val newGenePos = (Math.random * individual.chromosome.length).toInt
            // Get genes to swap
            val gene1 = individual.chromosome(newGenePos)
            val gene2 = individual.chromosome(geneIndex)
            // Swap genes
            individual.chromosome(geneIndex) = gene1
            individual.chromosome(newGenePos) = gene2
          }
        }
      }
      individual
    }.toArray
    new Population(individuals)
  }
}
