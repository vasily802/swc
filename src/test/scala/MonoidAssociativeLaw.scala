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

//  booleanOrMonoid.checkAssociativeLaw
//  booleanXorMonoid.checkAssociativeLaw
//  booleanNandMonoid.checkAssociativeLaw
//  booleanNorMonoid.checkAssociativeLaw
//  booleanNxorMonoid.checkAssociativeLaw

}
