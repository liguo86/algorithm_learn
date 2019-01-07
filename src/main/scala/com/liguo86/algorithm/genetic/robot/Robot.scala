package com.liguo86.algorithm.genetic.robot

import com.liguo86.algorithm.genetic.robot.Direction.Direction

import scala.collection.mutable

object Direction extends Enumeration {
  type Direction = Value
  val NORTH, EAST, SOUTH, WEST = Value
}

case class Robot(var sensorActions: Array[Int], maze: Maze, var maxMoves: Int, var xPosition: Int, var yPosition: Int, var sensorVal: Int = -1,
                 var heading: Direction = Direction.EAST, var moves: Int = 0, var route: mutable.MutableList[Array[Int]] = mutable.MutableList[Array[Int]]()) {

  def this(sensorActionsStr: Array[Int], maze: Maze, maxMoves: Int) = {
    this(sensorActionsStr, maze, maxMoves, -1, -1)
    this.sensorActions = calcSensorAction(sensorActionsStr)
    val startPos = maze.getStartPosition()
    xPosition = startPos(0)
    yPosition = startPos(1)
    this.route.+=(startPos)
  }

  def run(): Unit = {
    while (true) {
      this.moves += 1
      // Break if the robot stops moving
      if (this.getNextAction == 0)
        return
      // Break if we reach the goal
      if (this.maze.getPositionValue(this.xPosition, this.yPosition) == 4)
        return
      // Break if we reach a maximum number of moves
      if (this.moves > this.maxMoves) return
      // Run action
      this.makeNextAction()
    }
  }

  /**
    * 将机器人的传感器数据映射到二进制字符串的动作
    *
    * @param sensorActionStr
    * @return
    */
  def calcSensorAction(sensorActionStr: Array[Int]): Array[Int] = {
    val numActions = sensorActionStr.length / 2
    (0 to numActions - 1).map { sensorValue =>
      var sensorAction = 0
      if (sensorActionStr(sensorValue * 2) == 1) {
        sensorAction += 2
      }
      if (sensorActionStr(sensorValue * 2 + 1) == 1) {
        sensorAction += 1
      }
      sensorAction
    }.toArray
  }

  /**
    * 运行下一动作
    */
  def makeNextAction(): Unit = {
    // If move forward
    if (this.getNextAction() == 1) {
      val currentX = this.xPosition
      val currentY = this.yPosition

      // Move depending on current direction
      if (Direction.NORTH == this.heading) {
        this.yPosition += -1
        if (this.yPosition < 0) {
          this.yPosition = 0
        }
      }
      else if (Direction.EAST == this.heading) {
        this.xPosition += 1
        if (this.xPosition > this.maze.getMaxX()) {
          this.xPosition = this.maze.getMaxX()
        }
      }
      else if (Direction.SOUTH == this.heading) {
        this.yPosition += 1
        if (this.yPosition > this.maze.getMaxY()) {
          this.yPosition = this.maze.getMaxY()
        }
      }
      else if (Direction.WEST == this.heading) {
        this.xPosition += -1
        if (this.xPosition < 0) {
          this.xPosition = 0
        }
      }

      // We can't move here
      if (this.maze.isWall(this.xPosition, this.yPosition) == true) {
        this.xPosition = currentX
        this.yPosition = currentY
      }
      else {
        if (currentX != this.xPosition || currentY != this.yPosition) {
          this.route.+=(this.getPosition())
        }
      }
    }
    // Move clockwise
    else if (this.getNextAction() == 2) {
      if (Direction.NORTH == this.heading) {
        this.heading = Direction.EAST
      }
      else if (Direction.EAST == this.heading) {
        this.heading = Direction.SOUTH
      }
      else if (Direction.SOUTH == this.heading) {
        this.heading = Direction.WEST
      }
      else if (Direction.WEST == this.heading) {
        this.heading = Direction.NORTH
      }
    }
    // Move anti-clockwise
    else if (this.getNextAction() == 3) {
      if (Direction.NORTH == this.heading) {
        this.heading = Direction.WEST
      }
      else if (Direction.EAST == this.heading) {
        this.heading = Direction.NORTH
      }
      else if (Direction.SOUTH == this.heading) {
        this.heading = Direction.EAST
      }
      else if (Direction.WEST == this.heading) {
        this.heading = Direction.SOUTH
      }
    }

    // Reset sensor value
    this.sensorVal = -1
  }

  def getNextAction(): Int = {
    sensorActions(getSensorValue)
  }

  def getSensorValue: Int = {
    if (this.sensorVal > -1) {
      this.sensorVal
    } else {
      var (frontSensor, frontLeftSensor, frontRightSensor, leftSensor, rightSensor, backSensor) = (false, false, false, false, false, false)
      if (heading == Direction.NORTH) {
        frontSensor = maze.isWall(xPosition, yPosition - 1)
        frontLeftSensor = maze.isWall(xPosition - 1, yPosition - 1)
        frontRightSensor = maze.isWall(xPosition + 1, yPosition - 1)
        leftSensor = maze.isWall(xPosition - 1, yPosition)
        rightSensor = maze.isWall(xPosition + 1, yPosition)
        backSensor = maze.isWall(xPosition, yPosition + 1)
      } else if (heading == Direction.EAST) {
        frontSensor = maze.isWall(xPosition + 1, yPosition)
        frontLeftSensor = maze.isWall(xPosition + 1, yPosition - 1)
        frontRightSensor = maze.isWall(xPosition + 1, yPosition + 1)
        leftSensor = maze.isWall(xPosition, yPosition - 1)
        rightSensor = maze.isWall(xPosition, yPosition + 1)
        backSensor = maze.isWall(xPosition - 1, yPosition)
      } else if (heading == Direction.SOUTH) {
        frontSensor = maze.isWall(xPosition, yPosition + 1)
        frontLeftSensor = maze.isWall(xPosition + 1, yPosition + 1)
        frontRightSensor = maze.isWall(xPosition - 1, yPosition + 1)
        leftSensor = maze.isWall(xPosition + 1, yPosition)
        rightSensor = maze.isWall(xPosition - 1, yPosition)
        backSensor = maze.isWall(xPosition, yPosition - 1)
      } else {
        frontSensor = maze.isWall(xPosition - 1, yPosition)
        frontLeftSensor = maze.isWall(xPosition - 1, yPosition + 1)
        frontRightSensor = maze.isWall(xPosition - 1, yPosition - 1)
        leftSensor = maze.isWall(xPosition, yPosition + 1)
        rightSensor = maze.isWall(xPosition, yPosition - 1)
        backSensor = maze.isWall(xPosition + 1, yPosition)
      }

      var sensorVal = 0

      if (frontSensor == true) sensorVal += 1
      if (frontLeftSensor == true) sensorVal += 2
      if (frontRightSensor == true) sensorVal += 4
      if (leftSensor == true) sensorVal += 8
      if (rightSensor == true) sensorVal += 16
      if (backSensor == true) sensorVal += 32

      this.sensorVal = sensorVal
      sensorVal
    }
  }

  def getPosition(): Array[Int] = {
    Array(this.xPosition, this.yPosition)
  }

  private def getHeading = {
    this.heading
  }
}
