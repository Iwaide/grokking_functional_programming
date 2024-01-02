case class Point(x: Int, y: Int)
val xs = List(1)
val ys = List(-2, 7)

for {
  x <- xs
  y <- ys
}  yield Point(x, y)

case class Point3d(x: Int, y: Int, z: Int)
val xs = List(1)
val ys = List(-2, 7)
val zs = List(3, 4)

for {
  x <- xs
  y <- ys
  z <- zs
} yield Point3d(x, y, z)

xs.flatMap(x =>
  ys.flatMap(y =>
    zs.map(z =>
      Point3d(x, y, z)
    )
  )
)
