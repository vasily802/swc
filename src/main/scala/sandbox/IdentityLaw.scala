package sandbox

import scala.language.implicitConversions

object IdentityLaw {
  implicit def identityLaw[T](x: T)
                             (implicit m: Monoid[T]): Boolean = {
    (m.combine(x, m.empty) == x) &&
      (m.combine(m.empty, x) == x)
  }

  implicit def identityLawHigher[A[_], T](x: A[T])
                             (implicit m: Monoid[A[T]]): Boolean = {
    (m.combine(x, m.empty) == x) &&
      (m.combine(m.empty, x) == x)
  }

  implicit class IdentityLawOps[B](m: Monoid[B]) {
    def checkIdentityLaw(testValue: B)(implicit identityLaw: B => Boolean): Boolean =
      identityLaw(testValue)
  }

  implicit class IdentityLawOpsHigher[A[_], T](m: Monoid[A[T]]) {
    def checkIdentityLawHigher(testValue: A[T]): Boolean =
      identityLawHigher(testValue)(m)
  }
}
