package sandbox
import cats.{Monoid => CatsMonoid}
import cats.implicits._

object SuperAdder {
  def add[A: CatsMonoid](as: List[A]): A = as.foldLeft(CatsMonoid[A].empty)(_ |+| _)

  implicit val orderMonoid: CatsMonoid[Order] = new CatsMonoid[Order] {
    def combine(x: Order, y: Order): Order =
      Order(
        x.totalCost |+| y.totalCost,
        x.quantity |+| y.quantity
      )

    def empty: Order = Order(CatsMonoid[Double].empty, CatsMonoid[Double].empty)
  }
}

case class Order(totalCost: Double, quantity: Double)

// if we assume the regular monoid is "additive"
case class Mult[A](value: A) extends AnyVal

object Mult {
  implicit val intMult: Monoid[Mult[Int]] =
    new Monoid[Mult[Int]] {
      def empty: Mult[Int] = Mult(1)
      def combine(x: Mult[Int], y: Mult[Int]): Mult[Int] = Mult(x.value * y.value)
    }

}