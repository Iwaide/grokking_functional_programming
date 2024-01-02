case class Point(x: Int, y: Int)
val points = List(Point(5, 2), Point(1, 1))
val riskyRadius = List(-10, 0, 2)

def isInside(point: Point, radius: Int): Boolean = {
  radius * radius >= point.x * point.x + point.y * point.y
}

for {
  point <- points
  radius <- riskyRadius.filter(r => r > 0).filter(r => isInside(point, r))
} yield s"$point is within a radius of $radius "

for {
  radius <- riskyRadius.filter(r => r > 0)
  if radius > 0
  point <- points
  if isInside(point, radius)
} yield s"$point is within a radius of $radius "

def insideFilter(point: Point, r: Int): List[Point] = {
  if (isInside(point, r)) List(point) else List.empty
}

def validRadius(r: Int): List[Int] = {
  if (r > 0) List(r) else List.empty
}

for {
  point <- points
  radius <- riskyRadius
  validRadius <- validRadius(radius)
  inPoint <- insideFilter(point, radius)
} yield s"$point is within a radius of $radius "
