package richousrick.fileutilities.propmanager

import java.nio.file.Path
import java.util.Properties

import scala.reflect.runtime.universe.TypeTag

/**
 * Companion object for [[richousrick.fileutilities.propmanager.PropertiesInstance]].
 */
object PropertiesInstance {

	/**
	 * Creates a properties instance that has support for additional types
	 *
	 * @param additionalHandlers handlers supporting additional types.
	 *                           Note: handlers are used on a first found basis not best fit.
	 * @param typed              if the properties should be marked with their type. If false then conversions may be possible.
	 *                           For instance the value '1' could be loaded as a: character, string, or numeric type
	 */
	def apply(additionalHandlers: Set[TypeHandler[_]], typed: Boolean = true): PropertiesInstance = new PropertiesInstance(typed, additionalHandlers)

	/**
	 * Creates a simple properties instance that will use the global handlers
	 *
	 * @param typed if the properties should be marked with their type. If false then conversions may be possible.
	 *              For instance the value '1' could be loaded as a: character, string, or numeric type
	 */
	def apply(typed: Boolean): PropertiesInstance = new PropertiesInstance(typed)

	/**
	 * Creates a simple typed properties instance that will use the global handlers
	 */
	def apply(): PropertiesInstance = new PropertiesInstance(true)

}

/**
 * Class used to store a collection of properties that can be automatically loaded and written to the properties file.
 *
 * @param typed                if the properties should be marked with their type. If false then conversions may be possible.
 *                             For instance the value '1' could be loaded as a: character, string, or numeric type
 * @param customHandlers       optional set of TypeHandlers that either provide support for unsupported types, or replace the original handlers
 * @param ignoreHandlers       set of global handlers that should be ignored. This exists mainly to support overwriting global handlers in favour of specialised custom handlers.
 * @param ignoreGlobalHandlers if false then the global handlers not present in ignoreHandlers will be used in addition to the customHandlers.
 */
class PropertiesInstance(val typed: Boolean, val customHandlers: Set[TypeHandler[_]] = Set(), val ignoreHandlers: Set[TypeHandler[_]] = Set(), val ignoreGlobalHandlers: Boolean = false) extends Properties {


	/**
	 * Loads an instance from a file
	 *
	 * @param path path to the file the properties should be read from
	 * @return true, if properties were loaded successfully from the given file
	 */
	def load(path: Path): Boolean = PropertiesIO.readConfigSwallow(path, this).isDefined

	/**
	 * Writes the instance to a file
	 *
	 * @param path path to the file the properties should be written to
	 * @return true, if properties were saved successfully to the given file
	 */
	def write(path: Path): Boolean = PropertiesIO.writeConfigSwallow(path, this)

	/**
	 * Gets the handlers supported by this instance
	 *
	 * @return the handlers supported by this instance
	 */
	private def getHandlers: Set[TypeHandler[_]] = if (ignoreGlobalHandlers) customHandlers else (TypeHandler.globalHandlers -- ignoreHandlers) ++ customHandlers

	/**
	 * Attempts to add the specified key value pair to the properties list.
	 *
	 * @param name  name of the property to store
	 * @param value value of the property
	 * @param typed if the variable should be stored with its type.
	 * @param tt    TypeTag of type T
	 * @tparam T type of the property being stored
	 * @return the previous value of the specified property, if one existed
	 * @throws UnsupportedOperationException if no handlers support type T
	 */
	def setProperty[T](name: String, value: T, typed: Boolean = this.typed)
										(implicit tt: TypeTag[T]): Option[String] = TypeHandler
		.resolveHandler[T](getHandlers) match {
		case Some(handler) =>
			handler.writeProperty(name, value, this, typed)
		case None => throw new UnsupportedOperationException(s"No handler was found for type ${tt.tpe}")
	}

	/**
	 * Attempts to read a property from the properties list
	 *
	 * @param name  name of the property to read
	 * @param typed if the variable was stored with its type
	 * @param tt    TypeTag of type T
	 * @tparam T type of the property being loaded
	 * @return the value if it was loaded successfully; None otherwise
	 * @throws UnsupportedOperationException if no handlers support type T
	 */
	def getProperty[T](name: String, typed: Boolean = this.typed)(implicit tt: TypeTag[T]): Option[T] = {
		TypeHandler.resolveHandler[T](getHandlers) match {
			case Some(handler) => handler.loadProperty(name, this, typed)
			case None => throw new UnsupportedOperationException(s"No handler was found for type ${tt.tpe}")
		}
	}
}
