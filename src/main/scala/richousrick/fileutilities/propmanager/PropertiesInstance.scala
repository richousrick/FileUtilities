package richousrick.fileutilities.propmanager

import java.nio.file.Path
import java.util.Properties

import scala.reflect.runtime.universe.TypeTag

/**
 * Companion object for [[richousrick.fileutilities.propmanager.PropertiesInstance]].
 */
object PropertiesInstance {

	def apply(typed: Boolean = true, handlers: Set[TypeHandler[_]] = TypeHandler.defaultHandlers): PropertiesInstance = new PropertiesInstance(typed, handlers)
}

/**
 * Class used to store a collection of properties that can be automatically loaded and written to the properties file.
 *
 * @param typed if the properties should be marked with their type. If false then conversions may be possible.
 *              For instance the value '1' could be loaded as a: character, string, or numeric type
 */
class PropertiesInstance(private val typed: Boolean, handlers: Set[TypeHandler[_]]) extends Properties {

	/**
	 * Loads an instance from a file
	 *
	 * @param path path to the file the properties should be read from
	 */
	def load(path: Path): Unit = PropertiesIO.readConfigSwallow(path, this)

	/**
	 * Writes the instance to a file
	 *
	 * @param path path to the file the properties should be written to
	 */
	def write(path: Path): Unit = PropertiesIO.writeConfigSwallow(path, this)

	/**
	 * Attempts to add the specified key value pair to the properties list.
	 *
	 * @param name  name of the property to store
	 * @param value value of the property
	 * @param typed if the variable should be stored with its type.
	 * @param tt    TypeTag of type T
	 * @tparam T type of the property being stored
	 * @return the previous value of the specified property, if one existed
	 */
	def setProperty[T](name: String, value: T, typed: Boolean = this.typed)
										(implicit tt: TypeTag[T]): Option[String] = TypeHandler
		.resolveHandler[T](handlers) match {
		case Some(handler) =>
			handler.writeProperty(name, value, this, typed)
		case None => None
	}

	/**
	 * Attempts to read a property from the properties list
	 *
	 * @param name  name of the property to read
	 * @param typed if the variable was stored with its type
	 * @param tt    TypeTag of type T
	 * @tparam T type of the property being loaded
	 * @return the value if it was loaded successfully; None otherwise
	 */
	def getProperty[T](name: String, typed: Boolean = this.typed)(implicit tt: TypeTag[T]): Option[T] = {
		val handler = TypeHandler.resolveHandler[T](handlers)

		handler match {
			case Some(handler) => handler.loadProperty(name, this, typed)
			case None => None
		}
	}
}
