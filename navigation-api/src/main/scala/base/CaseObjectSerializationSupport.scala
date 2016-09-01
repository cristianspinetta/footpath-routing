package base

import spray.json._
import scala.reflect.ClassTag

/**
 * A custom version of the Spray DefaultJsonProtocol with an additional
 * formatter for ADT's of only case objects.
 */
trait CaseObjectSerializationSupport extends DefaultJsonProtocol {

  def caseObjectJsonFormat[T: ClassTag](objects: T*)(implicit tag: ClassTag[T]) = new RootJsonFormat[T] {
    /** A mapping from object names to the objects */
    private val mapping = objects.map(obj ⇒ key(obj) -> obj).toMap

    override def read(json: JsValue): T = (json match {
      case JsString(value) ⇒ mapping.get(value)
      case _               ⇒ None
    }).getOrElse(deserializationError(s"Unknown json value found when converting to $tag: $json"))

    /** The toString value of a case object is its name */
    override def write(value: T): JsValue = JsString(key(value))

    private def key(input: T): String = SimpleClassNameExtractor(input.getClass)
  }

}

object CaseObjectSerializationSupport extends CaseObjectSerializationSupport

object SimpleClassNameExtractor {
  def extractSimpleClassName(input: String) = input.split("\\.").last.split("\\$").last
  def apply[T](input: Class[T]): String = extractSimpleClassName(input.getName)
}
