package sandbox

trait IdentityLaw[A[_], B] {
  def checkIdentityLaw(m: A[B]): Boolean
}

object IdentityLaw {
  def identityLaw[A](x: A)
                    (implicit m: Monoid[A]): Boolean = {
    (m.combine(x, m.empty) == x) &&
      (m.combine(m.empty, x) == x)
  }

  implicit val booleanMonoidIdentityLaw: IdentityLaw[NamedMonoid, Boolean] =
    new IdentityLaw[NamedMonoid, Boolean] {
      def checkIdentityLaw(m: NamedMonoid[Boolean]): Boolean = {
        val listTrueFalse: List[Boolean] = List(true, false)
        val checks: List[Boolean] = for {
          x <- listTrueFalse
        } yield {
          val holds: Boolean = identityLaw(x)(m)
          println(s"For ${m.getName} with value $x identity law holds $holds")
          holds
        }

        val res: Boolean = checks forall identity
        val msg: String = if (res) {
          s"For ${m.getName} identity law holds true"
        } else {
          s"Beware! For ${m.getName} identity law does not hold true - so ${m.getName} IS NOT A MONOID!"
        }
        println(msg)

        res
      }
    }

  implicit class IdentityLawOps[A[_], B](m: A[B]) {
    def checkIdentityLaw(implicit l: IdentityLaw[A, B]): Boolean =
      l.checkIdentityLaw(m)
  }
}