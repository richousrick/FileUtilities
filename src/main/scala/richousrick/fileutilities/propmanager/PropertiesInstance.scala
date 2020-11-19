package richousrick.fileutilities.propmanager

import java.nio.file.Path
import java.util.Properties

import scala.reflect.runtime.universe.{TypeRef, TypeTag, typeOf}

/**
 * Companion object for [[richousrick.fileutilities.propmanager.PropertiesInstance]].
 */
object PropertiesInstance {

	/**
	 * Gets an instance of [[richousrick.fileutilities.propmanager.TypedProperty TypedProperty]] that can handle a desired datatype.
	 *
	 * @param tt type tag of the desired datatype
	 * @tparam T desired datatype
	 * @return a [[richousrick.fileutilities.propmanager.TypedProperty TypedProperty]] that can handle data T, or None if T is an unsupported datatype
	 */
	private def resolveHandler[T](implicit tt: TypeTag[T]): Option[TypedProperty[T]] = (tt.tpe match {
		case e if e <:< typeOf[Enumeration] => EnumProperty.makeInstance(tt.mirror
			.runtimeClass(tt.tpe)
			.asSubclass[Enumeration](classOf[Enumeration]))
		case v if v <:< typeOf[Enumeration#Value] =>
			EnumProperty.makeInstance(tt.mirror
				.runtimeClass(typeOf[T].widen.asInstanceOf[TypeRef].pre.typeSymbol.asClass)
				.asSubclass[Enumeration](classOf[Enumeration]))
		case s if s =:= typeOf[String] => StringProperty
		case c if c =:= typeOf[Char] => CharProperty
		case b if b =:= typeOf[Boolean] => BooleanProperty
		case b if b =:= typeOf[Byte] => ByteProperty
		case s if s =:= typeOf[Short] => ShortProperty
		case i if i =:= typeOf[Int] => IntProperty
		case l if l =:= typeOf[Long] => LongProperty
		case f if f =:= typeOf[Float] => FloatProperty
		case d if d =:= typeOf[Double] => DoubleProperty
		case _ => null
	}) match {
		case null => None
		case tp => Some(tp.asInstanceOf[TypedProperty[T]])
	}

	def apply(typed: Boolean = true): PropertiesInstance = new PropertiesInstance(typed)
}

/**
 * Class used to store a collection of properties that can be automatically loaded and written to the properties file.
 *
 * @param typed if the properties should be marked with their type. If false then conversions may be possible.
 *              For instance the value '1' could be loaded as a: character, string, or numeric type
 */
class PropertiesInstance(private val typed: Boolean) extends Properties {

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
	 * @return true if the property was successfully stored
	 */
	def setProperty[T](name: String, value: T, typed: Boolean = this.typed)
										(implicit tt: TypeTag[T]): Boolean = PropertiesInstance
		.resolveHandler[T] match {
		case Some(handler) =>
			handler.writeProperty(name, value, this, typed)
			true

		case None => false
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
		val handler = PropertiesInstance.resolveHandler[T]

		handler match {
			case Some(handler) => handler.loadProperty(name, this, typed)
			case None => None
		}
	}
}
