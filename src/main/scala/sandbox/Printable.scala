package com.hautelook.swc

trait Printable[A] {
  def format(value: A): String
}

object Printable {
  implicit val intPrintable: Printable[Int] =
    new Printable[Int] {
      def format(value: Int): String = value.toString
    }

  implicit val stringPrintable: Printable[String] =
    new Printable[String] {
      def format(value: String): String = value
    }

  implicit val catPrintable: Printable[Cat] =
    new Printable[Cat] {
      def format(value: Cat): String =
        s"${value.name.format(stringPrintable)} is a ${value.age.format} year-old ${value.color.format(stringPrintable)} cat."
    }

  implicit class PrintableOps[A](value: A) {
    def format(implicit p: Printable[A]): String =
      p.format(value)

    def print(implicit p: Printable[A]): Unit =
      println(p.format(value))
  }
}
