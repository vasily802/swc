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

  implicit val intAddMonoid: NamedMonoid[Int] = new NamedMonoid[Int] {
    def empty: Int = 0

    def combine(x: Int, y: Int): Int = x + y

    def getName: String = "intAddMonoid"
  }



  implicit def setUnionMonoid[A]: NamedMonoid[Set[A]] = new NamedMonoid[Set[A]] {
    def empty: Set[A] = Set.empty[A]

    def combine(x: Set[A], y: Set[A]): Set[A] = x ++ y

    def getName: String = "Set-Union-Monoid"
  }

  implicit def setSymDiffMonoid[A]: NamedMonoid[Set[A]] =
    new NamedMonoid[Set[A]] {
      def combine(a: Set[A], b: Set[A]): Set[A] =
        (a diff b) union (b diff a)
      def empty: Set[A] = Set.empty[A]

      def getName: String = "Set-SymDiff-Monoid"
    }

  implicit def setDiffMonoid[A]: NamedMonoid[Set[A]] =
    new NamedMonoid[Set[A]] {
      def combine(a: Set[A], b: Set[A]): Set[A] =
        a -- b
      def empty: Set[A] = Set.empty[A]

      def getName: String = "Set-Diff-Monoid"
    }

  implicit def setIntersectionSemigroup[A]: Semigroup[Set[A]] =
    new Semigroup[Set[A]] {
      def combine(a: Set[A], b: Set[A]) =
        a intersect b
    }
}
