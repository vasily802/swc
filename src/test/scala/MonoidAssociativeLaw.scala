import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import sandbox.Monoid
import sandbox.Monoid._

import scala.language.implicitConversions

object MonoidAssociativeLaw extends Properties("Monoid associative law") {
  def associativeLaw1[A](x: A, y: A, z: A)(m: Monoid[A]): Boolean = {
    m.combine(x, m.combine(y, z)) ==
      m.combine(m.combine(x, y), z)
  }

  implicit class AssociativeLawOps[A](m: Monoid[A]) {
    def checkAssociativeLaw(x: A, y: A, z: A): Boolean =
      associativeLaw1(x: A, y: A, z: A)(m)
  }

  property("booleanAndMonoid") = forAll { (a: Boolean, b: Boolean, c: Boolean) =>
    booleanAndMonoid.checkAssociativeLaw(a, b, c)
  }

  property("booleanOrMonoid") = forAll { (a: Boolean, b: Boolean, c: Boolean) =>
    booleanOrMonoid.checkAssociativeLaw(a, b, c)
  }

  property("booleanXorMonoid") = forAll { (a: Boolean, b: Boolean, c: Boolean) =>
    booleanXorMonoid.checkAssociativeLaw(a, b, c)
  }

  property("booleanNandMonoid") = forAll { (a: Boolean, b: Boolean, c: Boolean) =>
    booleanNandMonoid.checkAssociativeLaw(a, b, c)
  }

  property("booleanNorMonoid") = forAll { (a: Boolean, b: Boolean, c: Boolean) =>
    booleanNorMonoid.checkAssociativeLaw(a, b, c)
  }

  property("booleanNxorMonoid") = forAll { (a: Boolean, b: Boolean, c: Boolean) =>
    booleanNxorMonoid.checkAssociativeLaw(a, b, c)
  }
}
