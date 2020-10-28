package richousrick.fileutilities.lib

import scala.reflect.runtime.universe._

/**
 * Class used to store a collection of properties that can be automatically loaded and written to the properties file.
 */
class PropertiesInstance {

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
}
