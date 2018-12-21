package sandbox
import cats.{Monoid => CatsMonoid}
import cats.implicits._

object SuperAdder {
  def add[A: CatsMonoid](as: List[A]): A = as.foldLeft(CatsMonoid[A].empty)(_ |+| _)
}

case class Order(totalCost: Double, quantity: Double)