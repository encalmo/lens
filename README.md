# lens
Scala library providing Lens[R,V] typeclass derivation.


## Usage

```scala

    case class Person(firstName: String, lastName: String, address: Address)
    case class Address(street1: String, street2: Option[String] = None, postcode: String, town: String, country: String)

    val townLens = Lens[Person].address.town
    
    val mike = Person("Mike","Hart", Address("1 Abbey Road", None, "BN15 KJ", "Exeter", "United Kingdom"))

    val town =  townLens.get(mike)
    townLens.set(mike)("Derby")
    townLens.update(mike, town => town.toUpperCase())

```
