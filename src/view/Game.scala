package view

import logic.{Player, Square}
import processing.core.{PApplet, PConstants, PImage}
import structures._

// 1) Refactoring Done!!
// 2) Fix Spikes DONE!!
// 3) Winning Screen DONE!!
// 4) Map DONE!!
// 5) ReadMe
// 6) Video

class Game extends PApplet {

  private var currentPage : String = "Home"

  private val screenHeight: Int = 360
  private val screenWidth: Int = 640
  private val cellSize: Int = 40
  private var images : List[PImage] = List()

  private var currentPlayer : Option[Player] = None

  def sketch(objectType: Char, cellX: Int, cellY: Int, x: Int, y: Int, startWidth : Int, startHeight : Int) = {

    val cropX : Int = if (cellX < x + startWidth) x + startWidth - cellX else 0
    var cropY : Int = if (cellY < y + startHeight) y + startHeight - cellY else 0


    (objectType) match {
      case 'S' => {

        var spike = images(2)
        spike = spike.get(spike.width * cropX / cellSize, spike.height * cropY / cellSize, spike.width * (cellSize - cropX) / cellSize, spike.height * (cellSize - cropY) / cellSize)
        image(spike, x, y, cellSize - cropX, cellSize - cropY)
      }
      case 's' => {

        var spike = images(2)
        val movY = if (cropY < cellSize/2) cropY else cellSize/2
        cropY = if (cropY > cellSize/2) cropY - cellSize/2 else 0

        spike = spike.get(spike.width * cropX / cellSize, spike.height * cropY / cellSize, spike.width * (40 - cropX) / cellSize, spike.height * (cellSize - cropY) / cellSize)
        image(spike, x, y + cellSize/2 - movY, cellSize - cropX, cellSize/2 - cropY)
      }
      case 'B' => {

        var block = images(3)
        block = block.get(block.width * cropX / cellSize, block.height * cropY / cellSize,block.width * (cellSize - cropX) / cellSize, block.height * (cellSize - cropY) / cellSize)
        image(block, x, y, cellSize - cropX, cellSize - cropY)
      }
      case _ => {
      }
      //case 'D' => drawShards()
      //case 'j' => drawJump()
      //case 'J' => drawAirJump()
      //case 'r' => drawReverse()
      //case 'R' => drawAirReverse()
      //case 'p' => drawReversePortal()
      //case 'P' => drawShipPortal()
      //case 'X' => drawFloorBlock()
    }
  }

  def renderView(position: Point, rotation : Int, map : List[List[Char]]): Unit = {

    val cameraWidth: Int = 16 * cellSize
    val cameraHeight: Int = 9 * cellSize
    val mapWidth: Int = map.last.length * cellSize
    val mapHeight: Int = map.length * cellSize

    val startWidth: Int = 0.max((position.x - 80).min(mapWidth - cameraWidth))
    val startHeight: Int = 0.max((position.y - 240).min(mapHeight - cameraHeight))
    val finalWidth: Int = mapWidth.min(cameraWidth + startWidth)
    val finalHeight: Int = mapHeight.min(cameraHeight + startHeight)

    val floor = images(4)
    val floorSize = cellSize * 0.max(2 - startHeight)
    image(floor, 0, screenHeight - floorSize, screenWidth, floorSize)


    var coveredCoords: List[Point] = List()

    var playerShown : Boolean = false

    for (y <- startHeight  until finalHeight) {
      for (x <- startWidth until finalWidth) {

        if (!playerShown && x - startWidth == position.x - startWidth && y - startHeight == position.y) {
          playerShown = true
          val square = images(1)
          pushMatrix()
          translate(x - startWidth + 20, y - startHeight + 20)
          val angle : Float = (rotation * PConstants.PI) / 180
          //println("Angle: " + rotation)
          rotate(angle)
          image(square, -20, -20, 40, 40)
          popMatrix()
        }

        val cellX: Int = x / 40
        val cellY: Int = y / 40
        val cellPoint = Point(cellX, cellY)

        if (!coveredCoords.contains(cellPoint)) {

          coveredCoords = coveredCoords :+ cellPoint
          val objectType = map(cellY)(cellX)
          sketch(objectType, cellX * 40, cellY * 40, x - startWidth, y - startHeight, startWidth, startHeight)
        }

      }
    }

  }


  override def settings(): Unit = {
    size(screenWidth, screenHeight)
  }

  override def setup(): Unit = {


    val homeScreen : PImage = loadImage("src/images/homescreen.png")
    val startButton : PImage = loadImage("src/images/startbutton.png")
    val spike : PImage = loadImage("src/images/spike.png")
    val square : PImage = loadImage("src/images/square.png")
    val block : PImage = loadImage("src/images/block.png")
    val background = loadImage("src/images/background.png")
    val floor = loadImage("src/images/floor.png")

    images = List(background,square,spike,block, floor)

    image(homeScreen, 0, 0, screenWidth, screenHeight)
    image(startButton,270,250,100,100)

    frameRate(400)

  }

  def drawLevel() : Player = {
    //val background = images(0)
    //image(background, 0, 0, screenWidth, screenHeight)

    val currentPlayer : Player = Square(new GameMap().testMap)
    currentPlayer
  }

  override def draw(): Unit = {

    if (currentPage == "Home" && mousePressed
      && mouseX > 270 && mouseX < 370 && mouseY > 250 && mouseY < 350) {
        currentPage = "Game"
        currentPlayer = Some(drawLevel())
    } else {

      currentPlayer match {
        case Some(value) =>

          val background = images(0)
          image(background, 0, 0, screenWidth, screenHeight)

          if (value.position.x > cellSize * (value.map.head.size - 3)){
            textSize(40)
            textAlign(PConstants.CENTER, PConstants.CENTER)
            text("Level Completed!", width / 2, height / 2)
          }
          value.move(keyPressed)
          renderView(value.position, value.rotation, value.map)
        case None => null
      }
    }

  }

}

object Game {
  def main(args: Array[String]): Unit = {
    PApplet.main("view.Game")
  }
}
