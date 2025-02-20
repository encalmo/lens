# lens

Scala library providing Lens[R,V] typeclass derivation.

## Usage

Use with SBT

    libraryDependencies += "org.encalmo" % "lens_3" % "0.9.0"

or with SCALA-CLI

    //> using dep org.encalmo::lens:0.9.0

## Examples

```scala

    case class Person(firstName: String, lastName: String, address: Address)
    case class Address(street1: String, street2: Option[String] = None, postcode: String, town: String, country: String)

    val townLens = Lens[Person].address.town
    
    val mike = Person("Mike","Hart", Address("1 Abbey Road", None, "BN15 KJ", "Exeter", "United Kingdom"))

    val town =  townLens.get(mike)
    townLens.set(mike)("Derby")
    townLens.update(mike, town => town.toUpperCase())

```
