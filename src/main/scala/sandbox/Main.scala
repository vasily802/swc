package com.hautelook.swc

import JsonWriter._
import Printable._
import cats._
import cats.implicits._

object Main {
  def main(args: Array[String]): Unit = {
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

  }
}
