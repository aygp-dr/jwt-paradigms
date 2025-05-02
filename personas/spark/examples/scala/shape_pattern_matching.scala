sealed trait Shape
case class Circle(radius: Double) extends Shape
case class Rectangle(width: Double, height: Double) extends Shape
case class Triangle(base: Double, height: Double) extends Shape

def area(shape: Shape): Double = shape match {
  case Circle(r) => math.Pi * r * r
  case Rectangle(w, h) => w * h
  case Triangle(b, h) => 0.5 * b * h
}

// Test the area function
println(area(Circle(5.0)))
println(area(Rectangle(4.0, 3.0)))
println(area(Triangle(6.0, 2.0)))
