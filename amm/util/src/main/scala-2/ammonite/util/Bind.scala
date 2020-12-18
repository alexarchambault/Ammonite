package ammonite.util

import scala.reflect.runtime.universe.TypeTag

/**
 * Models a binding of a value to a typed name, and is passed into the
 * REPL so it can re-create the bindings inside the REPL's scope
 */
case class Bind[T](name: String, value: T, typeString: String)
object Bind{
  implicit def ammoniteReplArrowBinder[T](t: (String, T))(implicit typeTag: TypeTag[T]) = {
    Bind(t._1, t._2, typeTag.tpe.toString)
  }
}
