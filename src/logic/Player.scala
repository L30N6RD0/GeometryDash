package logic

import structures.Point
import processing.core._
import scala.math.{cos, sin}


abstract class Player(val map: List[List[Char]]) {
  var position : Point = Point(80, 240)
  var rotation : Int
  val objectType : String
  val cellSize : Int = 40

  def move(keyPressed : Boolean) : Point

}

case class Square(override val map: List[List[Char]]) extends Player(map) {

  override val objectType : String = "Square"

  var rotation : Int = 0
  var jump : Int = 0
  var drop : Int = 0
  var platform : Int = 0
  var onAir : Int = 0
  var axis : Int = 0

  private def squareCrash() : Boolean = {

    val corners : List[Point] = (for (x <- List(0,10,20,30,40)) yield for (y <- List(0,10,20,30,40)) yield Point(position.x + x, position.y + y)).flatten
    val axis : Point = Point(position.x + cellSize/2, position.y + cellSize/20)
    val squareCorners : List[Point] = for (corner : Point <- corners) yield {
      val radians = rotation * PConstants.PI / 180
      val newX : Int = ((corner.x - axis.x) * cos(radians)- (corner.y - axis.y) * sin(radians) + axis.x).toInt
      val newY : Int = ((corner.x - axis.x) * sin(radians) - (corner.y - axis.y) * cos(radians) + axis.y).toInt
      Point(newX, newY)
    }

    for (corner <- corners) {

      val cellsX : List[Int] =
        if (corner.x % cellSize == 0) {
          List(corner.x / cellSize, (corner.x + 1) / cellSize)
        } else List(corner.x / cellSize)

      val cellsY : List[Int] =
        if (corner.y % cellSize == 0) {
          List(corner.y / cellSize, (corner.y + 1) / cellSize)
        } else List(corner.y / cellSize)

      for (cellX <- cellsX) for (cellY <- cellsY) {
        val cell = Point(cellX * cellSize, cellY * cellSize)
        
        map(cellY)(cellX) match {
          case 'B' | 'X' =>
            if (cell.x < corner.x && corner.x < cell.x + cellSize && cell.y < corner.y && corner.y < cell.y + cellSize) return true
          //case 'S' =>
            //if (cell.x < corner.x && corner.x < cell.x + cellSize && cell.y < corner.y && corner.y < cell.y + cellSize &&
            //  corner.y < 2*corner.x + cell.y - 2*cell.x - cellSize && corner.y < -2*corner.x + cell.y + 2*cell.x + cellSize) return true
          case 'S' | 's' =>
            if (cell.x < corner.x && corner.x < cell.x + cellSize && cell.y < corner.y && corner.y < cell.y + cellSize &&
              corner.y < corner.x + cell.y - cell.x && corner.y < -1*corner.x + cell.y + cell.x + cellSize) return true
          case _ => 0
        }
      }
    }

    false
  }

  private def projectedFall(func : (Int) => Int): Int = {

    var trail : Int = 0
    var tryX : Int = position.x
    var tryY : Int = position.y
    val platform : Int = position.y

    do {

      trail += 10
      tryX += 10
      val prevY : Int = tryY
      val newY: Int = func(trail) + platform

      // if while dropping, between prev and new there's a multiple of 40, use it for squareLand
      tryY = if (prevY % cellSize != 0 && newY % cellSize != 0 && prevY / cellSize < newY / cellSize) {
        cellSize * (newY / cellSize)
      } else newY

    } while (!squareLand(tryX, tryY, false))

    trail
  }

  private def squareDrop(trail : Int): Int = trail*trail / 45
  private def squareJump(trail : Int): Int = (trail - 60) * (trail - 60) / 45 - 80

  private def squareFall(move : Int, func : (Int) => Int): Int = {

    if (move == 0) platform = position.y

    var trail: Int = move + 10
    val prevY: Int = position.y
    val newY: Int = func(trail) + platform

    position = Point(position.x, newY)

    // if while dropping, between prev and new there's a multiple of 40, use it for squareLand
    val tryY: Int = if (prevY % cellSize != 0 && newY % cellSize != 0 && prevY / cellSize < newY / cellSize) {
      cellSize * (newY / cellSize)
    } else newY

    if (squareLand(position.x, tryY, true)) {
      trail = 0
    }
    trail
  }

  private def squareLand(tryX : Int, tryY : Int, real : Boolean) : Boolean = {

    val backX : Int = tryX / 40
    val frontX : Int = (tryX + 39) / 40
    val cellY : Int = (tryY / 40) + 1
    for (y <- map.indices) for (x <- map(y).indices) {
      if ((backX == x || frontX == x) && cellY == y && (map(y)(x) == 'B' || map(y)(x) == 'X')) {
        if (real) position = Point(tryX, tryY)
        return true
      }
    }
    false
  }


  override def move(keyPressed : Boolean) : Point = {

    var keyPress : Boolean = keyPressed
    if (position.x < cellSize * (map.head.size - 3) && squareCrash()) {

      rotation = 0;
      jump = 0;
      drop = 0;
      position = Point(80, 240)
    }

    if (position.x > cellSize * (map.head.size - 3)){
      keyPress = false
    }

    position += Point(10,0)

    val landing : Boolean = squareLand(position.x,position.y, false)

    if (drop == 0 && (keyPress || jump != 0)) {
      if (jump == 0) {
        axis = rotation
        onAir = projectedFall(squareJump)
      }
      rotation = axis + 180 * (jump + 10) / onAir
      jump = squareFall(jump, squareJump)

    } else if (drop != 0 || !landing) {
      if (drop == 0) {
        axis = rotation
        onAir = projectedFall(squareDrop)
      }
      rotation = axis + 90 * (drop + 10) / onAir
      drop = squareFall(drop, squareDrop)

    }

    position
  }
}