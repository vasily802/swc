package com.hautelook.swc

trait Printable[A] {
  self =>

  def format(value: A): String

  def contramap[B](func: B => A): Printable[B] =
    new Printable[B] {
      def format(value: B): String = self.format(func(value))
    }
}

object Printable {
  import com.hautelook.swc.Printable.PrintableOps

  implicit val intPrintable: Printable[Int] =
    new Printable[Int] {
      def format(value: Int): String = value.toString
    }

  implicit val stringPrintable: Printable[String] =
    new Printable[String] {
      def format(value: String): String = "\"" + value + "\""
    }

  implicit def catPrintable: Printable[Cat] =
    stringPrintable.contramap(
      cat => {
        val name = cat.name.formatA
        val age = cat.age.format
        val color = cat.color.formatA
        s"$name is a $age year-old $color cat."
      }
    )

  implicit val booleanPrintable: Printable[Boolean] =
    new Printable[Boolean] {
      def format(value: Boolean): String =
        if (value) { "yes" } else { "no" }
    }

  implicit class PrintableOps[A](value: A) {
    def format(implicit p: Printable[A]): String =
      p.format(value)

    def print(implicit p: Printable[A]): Unit =
      println(p.format(value))

    def formatA(implicit p: Printable[A]): String = format // b/c of name collision
  }
}
