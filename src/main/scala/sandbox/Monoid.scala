package sandbox

trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

trait NamedMonoid[A] extends Monoid[A] {
  def getName: String
}

object Monoid {
  def apply[A](implicit monoid: Monoid[A]) =
    monoid

  implicit val booleanAndMonoid: NamedMonoid[Boolean] = new NamedMonoid[Boolean] {
    def empty: Boolean = true

    def combine(x: Boolean, y: Boolean): Boolean = x && y

    def getName: String = "Boolean-And-Monoid"
  }

  implicit val booleanOrMonoid: NamedMonoid[Boolean] = new NamedMonoid[Boolean] {
    def empty: Boolean = false

    def combine(x: Boolean, y: Boolean): Boolean = x || y

    def getName: String = "Boolean-Or-Monoid"
  }

  implicit val booleanXorMonoid: NamedMonoid[Boolean] = new NamedMonoid[Boolean] {
    def empty: Boolean = false

    def combine(x: Boolean, y: Boolean): Boolean = x ^ y

    def getName: String = "Boolean-Xor-Monoid"
  }

  implicit val booleanNandMonoid: NamedMonoid[Boolean] = new NamedMonoid[Boolean] {
    def empty: Boolean = true

    def combine(x: Boolean, y: Boolean): Boolean = !(x && y)

    def getName: String = "Boolean-Nand-Monoid"
  }

  implicit val booleanNorMonoid: NamedMonoid[Boolean] = new NamedMonoid[Boolean] {
    def empty: Boolean = true

    def combine(x: Boolean, y: Boolean): Boolean = !(x || y)

    def getName: String = "Boolean-Nor-Monoid"
  }

  implicit val booleanNxorMonoid: NamedMonoid[Boolean] = new NamedMonoid[Boolean] {
    def empty: Boolean = true

    def combine(x: Boolean, y: Boolean): Boolean = !(x ^ y)

    def getName: String = "Boolean-Nxor-Monoid"
  }
}
