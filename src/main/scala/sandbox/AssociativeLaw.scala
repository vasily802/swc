package sandbox

trait AssociativeLaw[A[_], B] {
  def checkAssociativeLaw(m: A[B]): Boolean
}

object AssociativeLaw {
  def associativeLaw[A](x: A, y: A, z: A)
                       (implicit m: Monoid[A]): Boolean = {
    m.combine(x, m.combine(y, z)) ==
      m.combine(m.combine(x, y), z)
  }

  implicit val booleanMonoidAssociativeLaw: AssociativeLaw[NamedMonoid, Boolean] =
    new AssociativeLaw[NamedMonoid, Boolean] {
      def checkAssociativeLaw(m: NamedMonoid[Boolean]): Boolean = {
        val listTrueFalse: List[Boolean] = List(true, false)
        val checks: List[Boolean] = for {
          x <- listTrueFalse
          y <- listTrueFalse
          z <- listTrueFalse
        } yield {
          val holds: Boolean = associativeLaw(x, y, z)(m)
          println(s"For ${m.getName} with values $x, $y, $z associative law holds $holds")
          holds
        }

        val res: Boolean = checks forall identity
        val msg: String = if (res) {
          s"For ${m.getName} associate law holds true"
        } else {
          s"Beware! For ${m.getName} associate law does not hold true - so ${m.getName} IS NOT A MONOID!"
        }
        println(msg)

        res
      }
    }

  implicit class AssociativeLawOps[A[_], B](m: A[B]) {
    def checkAssociativeLaw(implicit l: AssociativeLaw[A, B]): Boolean =
      l.checkAssociativeLaw(m)
  }
}