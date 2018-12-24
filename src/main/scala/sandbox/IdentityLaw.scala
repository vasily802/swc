package sandbox

import scala.language.implicitConversions

object IdentityLaw {
  implicit def identityLaw[T](x: T)
                             (implicit m: Monoid[T]): Boolean = {
    (m.combine(x, m.empty) == x) &&
      (m.combine(m.empty, x) == x)
  }

  implicit class IdentityLawOps[B](m: Monoid[B]) {
    def checkIdentityLaw(testValue: B)(implicit identityLaw: B => Boolean): Boolean =
      identityLaw(testValue)
  }

}