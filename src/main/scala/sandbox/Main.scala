package com.hautelook.swc

import JsonWriter._
import Printable._
import sandbox.{Monoid => SandboxMonoid}
import SandboxMonoid._
import sandbox.AssociativeLaw._
import sandbox.IdentityLaw._
import cats.implicits._

object Main {
  def main(args: Array[String]): Unit = {
    println
    /** Chapter I **/
    println(s"/** Chapter I **/")
    println
    val somePersonDave: Option[Person] = Some(Person("Dave", "dave@example.com"))
    val personMike = Person("Mike", "mike@example.com")

    println(
      Json.toJson(somePersonDave)
    )

    println(
      somePersonDave.toJson
    )

    println(
      personMike.toJson // extension method
    )

    val x: String = "Hello World"
    val y: Int = 123123

    x.print
    x.format(stringPrintable)

    y.print
    y.format

    // Define a cat:
    val cat = Cat("Fuzzy", 12, "black")

    // Print the cat!
    println(cat.format)

    val z: String = "string here".show

    println(z)

    println(cat.show)

    println(
      (Some(1): Option[Int]) === (None: Option[Int])
    )

    println(
      1.some === none[Int]
    )

    println(
      1.some =!= none[Int]
    )

    val cat1 = Cat("Garfield",   38, "orange and black")
    val cat2 = Cat("Heathcliff", 33, "orange and black")

    val optionCat1 = Option(cat1)
    val optionCat2 = Option.empty[Cat]

    println(
      cat1 === cat2
    )

    println(
      cat1 === cat1
    )

    println(
      optionCat1 =!= optionCat2
    )

    println(
      optionCat1 === optionCat1
    )

    println
    println(s"/** Chapter II **/")
    println

    booleanAndMonoid.checkAssociativeLaw
    booleanOrMonoid.checkAssociativeLaw
    booleanXorMonoid.checkAssociativeLaw
    booleanNandMonoid.checkAssociativeLaw
    booleanNorMonoid.checkAssociativeLaw
    booleanNxorMonoid.checkAssociativeLaw

    booleanAndMonoid.checkIdentityLaw
    booleanOrMonoid.checkIdentityLaw
    booleanXorMonoid.checkIdentityLaw
    booleanNandMonoid.checkIdentityLaw
    booleanNorMonoid.checkIdentityLaw
    booleanNxorMonoid.checkIdentityLaw

    /** End of def main */
  }
}
