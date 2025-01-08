package org.encalmo.lens

class LensSpec extends munit.FunSuite {

  case class TestCase1(foo: TestCase2, bar: Int, zoo: String, opt: Option[Int] = Some(0))

  case class TestCase2(faz: String, zaz: Int, opt2: Option[TestCase3] = None) {
    def withFaz(faz2: String) = this.copy(faz = faz2)
    def withZaz(zaz2: Int) = this.copy(zaz = zaz2)
  }

  case class TestCase3(tas: Boolean)

  val lens = Lens[TestCase1]
  val lens2 = lens.foo
  val lens3 = lens.foo.faz
  val lens4 = lens.foo.zaz
  val lens5 = lens.bar
  val lens6 = lens.zoo
  val lens7 = lens.opt
  val lens8 = lens.foo.opt2

  val sample = TestCase1(foo = TestCase2(faz = "Faz", zaz = 9), bar = 1, zoo = "Zoo")

  test("Lens should get value using lenses") {
    assertEquals(lens2.get(sample).faz, "Faz")
    assertEquals(lens2.get(sample).zaz, 9)
    assertEquals(lens3.get(sample), "Faz")
    assertEquals(lens4.get(sample), 9)
    assertEquals(lens5.get(sample), 1)
    assertEquals(lens6.get(sample), "Zoo")
    assertEquals(lens7.get(sample), Some(0))
    assertEquals(lens8.get(sample), None)
  }

  test("Lens should set value using lenses") {
    assertEquals(lens2.set(sample)(TestCase2(faz = "zaf", zaz = 8)).foo.faz, "zaf")
    assertEquals(
      lens2.set(sample)(TestCase2(faz = "zaf", zaz = 8)),
      TestCase1(
        foo = TestCase2(faz = "zaf", zaz = 8),
        bar = 1,
        zoo = "Zoo"
      )
    )
    assertEquals(lens3.set(sample)("zaf").foo.faz, "zaf")
    assertEquals(lens4.set(sample)(99).foo.zaz, 99)
    assertEquals(lens5.set(sample)(2).bar, 2)
    assertEquals(lens6.set(sample)("Hello").zoo, "Hello")
    assertEquals(lens7.set(sample)(None).opt, None)
    assertEquals(lens8.set(sample)(Some(TestCase3(true))).foo.opt2.map(_.tas), Some(true))
  }

  test("Lens should update value using lenses") {
    assertEquals(lens2.update(sample, _.withFaz("oooo")).foo.faz, "oooo")
    assertEquals(lens2.update(sample, _.withZaz(1000)).foo.zaz, 1000)
    assertEquals(lens2.update(sample, _.withZaz(1000)).foo.faz, "Faz")
    assertEquals(lens3.update(sample, _.reverse).foo.faz, "zaF")
    assertEquals(lens4.update(sample, _ * -1).foo.zaz, -9)
    assertEquals(lens5.update(sample, _ * -1).bar, -1)
    assertEquals(lens6.update(sample, _.reverse).zoo, "ooZ")
  }

  case class Person(firstName: String, lastName: String, address: Address)
  case class Address(street1: String, street2: Option[String] = None, postcode: String, town: String, country: String)

  test("Lens should set and update value using lenses") {
    val townLens = Lens[Person].address.town
    val mike = Person("Mike", "Hart", Address("1 Abbey Road", None, "BN15 KJ", "Exeter", "United Kingdom"))
    val mike2 = townLens.set(mike)("Derby")
    val town = townLens.get(mike)
    assertEquals(town, "Exeter")
    townLens.update(mike, town => town.toUpperCase())
    assertEquals(mike2.address.town, "Derby")
  }

  test("Lens should set value using set function from the lenses") {
    val setTown: Person => String => Person = Lens[Person].address.town.set
    val mike = Person("Mike", "Hart", Address("1 Abbey Road", None, "BN15 KJ", "Exeter", "United Kingdom"))
    val mike2 = setTown(mike)("Derby")
    assertEquals(mike2.address.town, "Derby")
  }

}
