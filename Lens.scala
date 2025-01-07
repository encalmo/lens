package org.encalmo.lens

import scala.NamedTuple.AnyNamedTuple
import scala.compiletime.*

/** Lens typeclass provides set of functions to retrieve and update potentially deeply-nested property of an object. */
trait Lens[R, V] {
  def get(root: R): V
  def set(root: R, value: V): R
  def update(root: R, update: V => V): R
}

object Lens {

  /** Root mount from which particular Lens instances can be derived for available properties. */
  final class LensMount[R <: Product] extends Selectable {

    type Fields = NamedTuple.Map[NamedTuple.From[R], [X] =>> LensOf[R, R, X]]

    transparent inline def selectDynamic(name: String & Singleton) =
      type X = TypeByName[NamedTuple.From[R], name.type]
      inline erasedValue[X] match {
        case _: Unit => error(s"$name is not a valid property name")
        case _: Product =>
          LensMacro.createLens[R, R, X & Product, AdjustableLens[R, R, X & Product]](name, identity[R], (_, r) => r)
        case _ =>
          LensMacro.createLens[R, R, X, FixedLens[R, R, X]](name, identity[R], (_, r) => r)
      }
  }

  /** Lens instance offering possibility to further select more fine-grained properties. */
  abstract class AdjustableLens[R, T <: Product, V <: Product] extends Lens[R, V] with Selectable {

    type Fields = NamedTuple.Map[NamedTuple.From[V], [X] =>> LensOf[R, V, X]]

    transparent inline def selectDynamic(name: String & Singleton) =
      type X = TypeByName[NamedTuple.From[V], name.type]
      inline erasedValue[X] match {
        case _: Unit => error(s"$name is not a valid property name")
        case _: Product =>
          LensMacro.createLens[R, V, X & Product, AdjustableLens[R, V, X & Product]](name, get, set)
        case _ =>
          LensMacro.createLens[R, V, X, FixedLens[R, V, X]](name, get, set)
      }
  }

  /** Lens instance fixed on some final property. */
  abstract class FixedLens[R, T <: Product, V] extends Lens[R, V]

  type LensOf[R, V, X] =
    X match {
      case Product => AdjustableLens[R, V, X]
      case _       => FixedLens[R, V, X]
    }

  type TypeByName[T <: AnyNamedTuple, Label <: String & Singleton] =
    Find[Tuple.Zip[NamedTuple.Names[T], NamedTuple.DropNames[T]], Label]

  type Find[T <: Tuple, Label <: String & Singleton] =
    T match {
      case EmptyTuple      => Unit
      case (Label, v) *: _ => v
      case _ *: t          => Find[t, Label]
    }

  inline def apply[R <: Product]: LensMount[R] = new LensMount[R]

}
