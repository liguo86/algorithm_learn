package com.liguo86.algorithm.genetic

/**
  *
  * @param populationSize 人口规模
  * @param mutationRate   变异率 （范围0-1，一般比较小，如0.1或者更小）
  * @param crossoverRate  交换率 （范围0-1）
  * @param elitismCount   精英计数 （精英是不需要进行交换和变异的直接进入下一代）
  */
class GeneticAlgorithm(val populationSize: Int, val mutationRate: Double, val crossoverRate: Double, val elitismCount: Int) {
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
    * @return
    */
  def calcFitness(individual: Individual): Double = {
    val fitness = individual.chromosome.sum * 1.0 / individual.chromosome.length
    individual.fitness = fitness
    fitness
  }

  /**
    * 评估整个群体的适应度（健康度）
    *
    * @param population
    */
  def evalPopulation(population: Population) = {
    val populationFitness = population.population.map(individual => calcFitness(individual)).sum
    population.populationFitness = populationFitness
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
    * 选择一个父亲用于交叉，用轮盘赌的方法
    *
    * @param population
    * @return
    */
  def selectParent(population: Population): Individual = {
    val individuals = population.population

    // 旋转轮盘赌
    val populationFitness = population.populationFitness
    val rouletteWheelPosition = Math.random * populationFitness

    // 找父亲
    var spinWheel = 0d
    for (individual <- individuals) {
      spinWheel += individual.fitness
      if (spinWheel >= rouletteWheelPosition) return individual
    }
    return individuals(population.population.length - 1)
  }

  /**
    * 交叉
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
        val offspring = (0 to parent1.chromosome.length - 1).map { geneIndex =>
          if (0.5 > Math.random()) {
            parent1.chromosome(geneIndex)
          } else {
            parent2.chromosome(geneIndex)
          }
        }.toArray[Int]
        newPopulation.setIndividual(i, new Individual(offspring))
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
            if (individual.chromosome(geneIndex) == 1) {
              0
            } else {
              1
            }
          } else {
            individual.chromosome(geneIndex)
          }
        }.toArray[Int]
        new Individual(chromosome)
      } else {
        individual
      }
    }.toArray[Individual]
    new Population(individuals)

    //    // Initialize new population
    //    val newPopulation = new Population(this.populationSize)
    //
    //    // Loop over current population by fitness
    //    var populationIndex = 0
    //    while ( {
    //      populationIndex < population.population.length
    //    }) {
    //      val individual = population.getFittest(populationIndex)
    //      // Loop over individual's genes
    //      var geneIndex = 0
    //      while ( {
    //        geneIndex < individual.chromosome.length
    //      }) { // Skip mutation if this is an elite individual
    //        if (populationIndex > this.elitismCount) { // Does this gene need mutation?
    //          if (this.mutationRate > Math.random) { // Get new gene
    //            var newGene = 1
    //            if (individual.chromosome(geneIndex) == 1) newGene = 0
    //            // Mutate gene
    //            individual.chromosome.update(geneIndex, newGene)
    //          }
    //        }
    //
    //        {
    //          geneIndex += 1; geneIndex - 1
    //        }
    //      }
    //      // Add individual to population
    //      newPopulation.setIndividual(populationIndex, individual)
    //
    //      {
    //        populationIndex += 1; populationIndex - 1
    //      }
    //    }
    //
    //    // Return mutated population
    //    return newPopulation
  }
}
