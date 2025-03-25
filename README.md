<a href="https://github.com/encalmo/lens">![GitHub](https://img.shields.io/badge/github-%23121011.svg?style=for-the-badge&logo=github&logoColor=white)</a> <a href="https://central.sonatype.com/artifact/org.encalmo/lens_3" target="_blank">![Maven Central Version](https://img.shields.io/maven-central/v/org.encalmo/lens_3?style=for-the-badge)</a> <a href="https://encalmo.github.io/lens/scaladoc/org/encalmo/lens.html" target="_blank"><img alt="Scaladoc" src="https://img.shields.io/badge/docs-scaladoc-red?style=for-the-badge"></a>

# lens

Scala library providing Lens[R,V] typeclass derivation.

## Table of contents

- [Dependencies](#dependencies)
- [Usage](#usage)
- [Examples](#examples)

## Dependencies

   - [Scala](https://www.scala-lang.org) >= 3.6.2

## Usage

Use with SBT

    libraryDependencies += "org.encalmo" % "lens_3" % "0.9.0"

or with SCALA-CLI

    //> using dep org.encalmo::lens:0.9.2

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


## Project content

```
├── .github
│   └── workflows
│       ├── pages.yaml
│       ├── release.yaml
│       └── test.yaml
│
├── .gitignore
├── .scalafmt.conf
├── Lens.scala
├── LensMacro.scala
├── LensSpec.test.scala
├── LICENSE
├── project.scala
├── README.md
└── test.sh
```