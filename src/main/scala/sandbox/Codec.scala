package sandbox

import Printable._

trait Codec[A] {
  self =>

  def encode(value: A): String
  def decode(value: String): A
  def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
    override def encode(value: B): String =
      self.encode(enc(value))

    override def decode(value: String): B =
      dec(self.decode(value))
  }
}

object Codec {
  def encode[A](value: A)(implicit c: Codec[A]): String =
    c.encode(value)

  def decode[A](value: String)(implicit c: Codec[A]): A =
    c.decode(value)

  implicit val stringCodec: Codec[String] =
    new Codec[String] {
      def encode(value: String): String = value
      def decode(value: String): String = value
    }

  implicit val doubleCodec: Codec[Double] = stringCodec.imap(
    _.toDouble, _.toString
  )

  implicit val catCodec: Codec[Cat] = stringCodec.imap(
    s => {
      // example: "Fuzzy|12|Black"
      val strLst: Seq[String] = s.split("|".toCharArray).toList
      strLst match {
        case n :: a :: c :: Nil =>
          Cat(n, a.toInt, c)
      }
    },
    cat => {
      val name = cat.name.formatA
      val age = cat.age.format
      val color = cat.color.formatA
      s"$name is a $age year-old $color cat."
    }
  )

  implicit class CodecOps[A](value: A) {
    def encode(implicit c: Codec[A]): String = c.encode(value)

    def decode[B](implicit cb: Codec[B], c: Codec[A]): B = {
      val s: String = value.encode
      cb.decode(s)
    }
  }
}
