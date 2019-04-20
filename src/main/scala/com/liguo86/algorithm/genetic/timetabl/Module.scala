package com.liguo86.algorithm.genetic.timetabl

case class Module(moduleId: Int, moduleCode: String, module: String, professorIds: Array[Int]) {
  def getRandomProfessorId(): Int = {
    professorIds(professorIds.length * Math.random())
  }
}
