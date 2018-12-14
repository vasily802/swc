package com.hautelook.swc

import cats._
import cats.implicits._

final case class Cat(name: String, age: Int, color: String)

object Cat {
  implicit val showCat: Show[Cat] = {
    new Show[Cat] {
      def show(t: Cat): String = s"${t.name.show} is a ${t.age.show} year old ${t.color.show} cat"
    }
  }

  implicit val eqCat: Eq[Cat] = Eq.instance[Cat] {
    (x: Cat, y: Cat) =>
      x.name === y.name &&
      x.age === y.age &&
      x.color === y.color
  }
}
