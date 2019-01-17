import sandbox.Monoid
import sandbox.Monoid._

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

import scala.language.implicitConversions

object MonoidIdentityLaw extends Properties("Monoid identity law") {
  implicit def identityLaw[T](x: T)(m: Monoid[T]): Boolean = {
    (m.combine(x, m.empty) == x) &&
      (m.combine(m.empty, x) == x)
  }

  implicit class IdentityLawOps[B](m: Monoid[B]) {
    def checkIdentityLaw(testValue: B): Boolean =
      identityLaw(testValue)(m)
  }

  property("booleanAndMonoid") = forAll { a: Boolean =>
    booleanAndMonoid.checkIdentityLaw(a)
  }

  property("booleanOrMonoid") = forAll { a: Boolean =>
    booleanOrMonoid.checkIdentityLaw(a)
  }

  property("booleanXorMonoid") = forAll { a: Boolean =>
    booleanXorMonoid.checkIdentityLaw(a)
  }

  property("booleanNandMonoid") = forAll { a: Boolean =>
    booleanNandMonoid.checkIdentityLaw(a)
  }

  property("booleanNorMonoid") = forAll { a: Boolean =>
    booleanNorMonoid.checkIdentityLaw(a)
  }

  property("booleanNxorMonoid") = forAll { a: Boolean =>
    booleanNxorMonoid.checkIdentityLaw(a)
  }

//  // todo: correct way to do this
//  property("setUnionMonoid") = forAll { a: Set[Int] =>
//    setUnionMonoid[Int].checkIdentityLaw(a)
//  }
}
