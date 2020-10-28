package richousrick.fileutilities.lib

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
}
