package structures

case class Point(x : Int, y : Int) {
  def +(rhs: Point): Point = Point(x + rhs.x, y + rhs.y)
  def -(rhs: Point): Point = Point(x - rhs.x, y - rhs.y)
}
