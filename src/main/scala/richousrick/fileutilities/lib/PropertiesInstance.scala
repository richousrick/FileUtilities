package richousrick.fileutilities.lib

import scala.reflect.runtime.universe._

/**
 * Companion object for [[richousrick.fileutilities.lib.PropertiesInstance]].
 */
object PropertiesInstance {

	/**
	 * Converts a property of type T to a string
	 *
	 * @param property value of parameter to convert to
	 * @return a string representing the property
	 */
	def write[T](property: T): String = property.toString

	/**
	 * Attempts to read a value from a given enum
	 *
	 * @param value name of the value to read. Note this is case sensitive.
	 * @tparam T Enum to load the value of
	 * @return The value of the enum with the specified name. If one exists
	 */
	def readEnum[T <: Enumeration : TypeTag](value: String): Option[T#Value] =
		runtimeMirror(getClass.getClassLoader).reflectModule(typeOf[T].termSymbol.asModule)
			.instance
			.asInstanceOf[T]
			.values
			.find(_.toString == value)


	/**
	 * Attempts to read a value from a string
	 *
	 * @param value to read as a string
	 * @tparam T type to parse the string to
	 * @return the value of the type T represented by the value; If one exists.
	 */
	def readVal[T <: AnyVal : TypeTag](value: String): Option[T] = try {
		typeOf[T] match {
			case t if t =:= typeOf[Boolean] => Some(value.toBoolean.asInstanceOf[T])
			case t if t =:= typeOf[Byte] => Some(value.toByte.asInstanceOf[T])
			case t if t =:= typeOf[Short] => Some(value.toShort.asInstanceOf[T])
			case t if t =:= typeOf[Int] => Some(value.toInt.asInstanceOf[T])
			case t if t =:= typeOf[Long] => Some(value.toLong.asInstanceOf[T])
			case t if t =:= typeOf[Float] => Some(value.toFloat.asInstanceOf[T])
			case t if t =:= typeOf[Double] => Some(value.toDouble.asInstanceOf[T])
			case t if value.length == 1 && t =:= typeOf[Char] => Some(value.charAt(0).asInstanceOf[T])
			case _ => None
		}
	} catch {
		case _: NumberFormatException | _: IllegalArgumentException => None
	}
}

/**
 * Class used to store a collection of properties that can be automatically loaded and written to the properties file.
 */
class PropertiesInstance {
}
