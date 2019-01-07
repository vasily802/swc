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

// Name is a type that isn't used at the value level: this is called a *phantom* type
case class Named[A, Name](value: A)

object Named {
  def create[Name]: NamedBuilder[Name] = new NamedBuilder[Name]

  class NamedBuilder[Name] {
    def apply[A](a: A): Named[A, Name] = new Named(a)
  }
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

  trait Union
  trait SymDiff

  type Smth1[A] = Named[Set[A], Union]
  type Smth2[A] = Named[Set[A], SymDiff]

  implicit def setUnionMonoid[A]: NamedMonoid[Smth1[A]] = new NamedMonoid[Smth1[A]] {
    def empty: Smth1[A] = Named(Set.empty[A])

    def combine(x: Smth1[A], y: Smth1[A]): Smth1[A] = Named(x.value ++ y.value)

    def getName: String = "Set-Union-Monoid"
  }

  implicit def setSymDiffMonoid[A]: NamedMonoid[Smth2[A]] =
    new NamedMonoid[Smth2[A]] {
      def combine(a: Smth2[A], b: Smth2[A]): Smth2[A] =
        Named((a.value diff b.value) union (b.value diff a.value))
      def empty: Smth2[A] = Named(Set.empty[A])

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
