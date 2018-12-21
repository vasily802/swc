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

    def empty: Order = Order(CatsMonoid[Double].empty, CatsMonoid[Double].empty) // TODO: how does it know which monoid on Double?
  }
}

case class Order(totalCost: Double, quantity: Double)
