package richousrick.fileutilities.lib

import scala.reflect.runtime.universe.{TypeTag, typeOf}

/**
 * Companion object for [[richousrick.fileutilities.lib.PropertiesInstance]].
 */
object PropertiesInstance {

	/**
	 * Gets an instance of [[richousrick.fileutilities.lib.TypedProperty TypedProperty]] that can handle a desired datatype.
	 *
	 * @param tt type tag of the desired datatype
	 * @tparam T desired datatype
	 * @return a [[richousrick.fileutilities.lib.TypedProperty TypedProperty]] that can handle data T, or None if T is an unsupported datatype
	 */
	private def resolveHandler[T](implicit tt: TypeTag[T]): Option[TypedProperty[T]] = (tt.tpe match {
		case e if e <:< typeOf[Enumeration] =>
			// Get base class and cast to a subclass of Enumeration.
			// Then pass that class to a function that will build an instance
			// Note: 	Type checking fails when extracting the type from the class
			// 				So a function is used that extracts the type from the class and uses that
			def makeInstance[E <: Enumeration : TypeTag](c: Class[E]): EnumProperty[E] = EnumProperty[E]

			makeInstance(tt.mirror.runtimeClass(tt.tpe).asSubclass[Enumeration](classOf[Enumeration]))
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
}

/**
 * Class used to store a collection of properties that can be automatically loaded and written to the properties file.
 */
class PropertiesInstance {
}
