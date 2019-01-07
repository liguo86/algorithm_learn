package com.liguo86.algorithm.genetic.robot

import Array._
import scala.collection.mutable

case class Maze(val maze: Array[Array[Int]], var startPoint: Array[Int] = Array(-1, -1)) {
  def getStartPosition(): Array[Int] = {
    if (startPoint(0) != -1 && startPoint(1) != -1) {
      return startPoint
    } else {
      (0 to maze.size-1).foreach { rowIdx =>
        val colSize = maze(rowIdx).length -1
        (0 to colSize).foreach { colIdx =>
          if (maze(rowIdx)(colIdx) == 2) {
            startPoint = Array(colIdx, rowIdx)
            return startPoint
          }
        }
      }
      Array(0, 0)
    }
  }

  def getPositionValue(x: Int, y: Int): Int = {
    if (x < 0 || y < 0 || x >= maze.size || y >= maze(0).size) {
      1
    } else {
      maze(y)(x)
    }
  }

  def isWall(x: Int, y: Int) = {
    getPositionValue(x, y) == 1
  }

  def getMaxX() = {
    maze(0).size - 1
  }

  def getMaxY() = {
    maze.size - 1
  }

  /**
    * 检查给定的路由得分
    *
    * @param route
    * @return
    */
  def scoreRoute(route: mutable.MutableList[Array[Int]]) = {
    var score = 0
    val visited = ofDim[Boolean](getMaxY + 1, getMaxX + 1)
    route.foreach { step =>
      if (maze(step(1))(step(0)) == 3 && !visited(step(1))(step(0))) {
        score = score + 1
        visited(step(1))(step(0)) = true
      }
    }
    score
  }
}
object Maze{
  def main(args: Array[String]): Unit = {
    val a = "00010000000101111110110001011010010101111111101101110110011001110001011000010101000101100111010100000111100001110011001001000110"
    val aa = a.toCharArray
    val chromosome = new Array[Int](aa.length)
    var i = 0
    (0 to aa.length-1).foreach{ i=>
      chromosome(i) = Integer.valueOf(String.valueOf(aa(i)))
    }
    val maze = new Maze(Array[Array[Int]](Array(0, 0, 0, 0, 1, 0, 1, 3, 2), Array(1, 0, 1, 1, 1, 0, 1, 3, 1), Array(1, 0, 0, 1, 3, 3, 3, 3, 1), Array(3, 3, 3, 1, 3, 1, 1, 0, 1), Array(3, 1, 3, 3, 3, 1, 1, 0, 0), Array(3, 3, 1, 1, 1, 1, 0, 1, 1), Array(1, 3, 0, 1, 3, 3, 3, 3, 3), Array(0, 3, 1, 1, 3, 1, 0, 1, 3), Array(1, 3, 3, 3, 3, 1, 1, 1, 4)))
    val robot = new Robot(chromosome, maze, 100)
    robot.run()
    val fitness = maze.scoreRoute(robot.route)
    println(fitness)

    val heading = Direction.SOUTH
    println(Direction.SOUTH eq heading)
    println(Direction.SOUTH == heading)
  }
}
