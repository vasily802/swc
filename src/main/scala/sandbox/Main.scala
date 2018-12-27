package com.hautelook.swc

// TODO: Is there a penalty on too many/too broad imports?
import JsonWriter._
import Printable._
import sandbox.{AssociativeLaw, IdentityLaw, NamedMonoid, SuperAdder, Monoid => SandboxMonoid}
import SandboxMonoid._
import sandbox.AssociativeLaw._
import sandbox.IdentityLaw._
import cats.implicits._

object Main {
  def main(args: Array[String]): Unit = {
    def chapter1(): Unit = {
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
      val catMore = Cat("Fuzzy1", 121, "black1")

      // Print the cat!
      println(cat.format)

      val z: String = "string here".show

      println(z)

      println(catMore.show)

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
    }

    chapter1

    def chapter2(): Unit = {
      println
      println(s"/** Chapter II **/")
      println
      println
      println(s"/** Exercise 2.3 **/")
      println
      booleanAndMonoid.checkAssociativeLaw
      booleanOrMonoid.checkAssociativeLaw
      booleanXorMonoid.checkAssociativeLaw
      booleanNandMonoid.checkAssociativeLaw
      booleanNorMonoid.checkAssociativeLaw
      booleanNxorMonoid.checkAssociativeLaw

      booleanAndMonoid.checkIdentityLaw(false)
      booleanOrMonoid.checkIdentityLaw(false)
      booleanXorMonoid.checkIdentityLaw(false)
      booleanNandMonoid.checkIdentityLaw(false)
      booleanNorMonoid.checkIdentityLaw(false)
      booleanNxorMonoid.checkIdentityLaw(false)

      println
      println(s"/** Exercise 2.4 **/")
      println

//      setUnionMonoid[Int].checkIdentityLawHigher(Set(1))
      setUnionMonoid[Int].checkAssociativeLaw(setMonoidAssociativeLaw.asInstanceOf[AssociativeLaw[NamedMonoid, Set[Int]]])

//      setSymDiffMonoid[Int].checkIdentityLaw(Set(1))
      setSymDiffMonoid[Int].checkAssociativeLaw(setMonoidAssociativeLaw.asInstanceOf[AssociativeLaw[NamedMonoid, Set[Int]]])

//      setDiffMonoid[Int].checkIdentityLaw(Set(1))
      setDiffMonoid[Int].checkAssociativeLaw(setMonoidAssociativeLaw.asInstanceOf[AssociativeLaw[NamedMonoid, Set[Int]]])

      val addResult1 = SuperAdder.add(List(Some(1), None))
      println(addResult1)

      val addResult2 = SuperAdder.add(List[Option[Int]](Some(1), Some(1)))
      println(addResult2)

      import sandbox.Order
      import SuperAdder._
      val o1 = Order(1,1)
      val o2 = Order(2,2)
      val ot = SuperAdder.add(List(o1, o2))
      println(ot)
    }

    chapter2

    def chapter3(): Unit = {
      import sandbox.{Tree=> SandboxTree}

      val tree = SandboxTree.branch(
        SandboxTree.branch(
          SandboxTree.leaf(1),SandboxTree.leaf(1)
        ),
        SandboxTree.branch(
          SandboxTree.leaf(1),SandboxTree.leaf(1)
        )
      )

      val mappedTree = tree.map(_ * 2)

      println(mappedTree)
    }

    chapter3
    /** End of def main */
  }
}
