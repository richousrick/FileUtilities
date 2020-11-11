package richousrick.fileutilities.propmanager

import java.util.Properties

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.runtimeMirror


/**
 * Base class enabling parsing of types to and from Strings
 *
 * @tparam T base type handled by the TypedProperty instance
 */
sealed abstract class TypedProperty[T] {

	/**
	 * Prefix for property name
	 *
	 * @return a prefix used to identify the property type
	 */
	def prefix: Char

	/**
	 * Attempts to parse the given value to an instance of type T
	 *
	 * @param value value to attempt to instantiate.
	 * @return an instance of type T, If one can be created.
	 */
	def load(value: String): Option[T]

	/**
	 * Attempts to load a property with the given name as an instance of type T
	 *
	 * @param name       of the property to load
	 * @param properties collection of properties to load the desired value from from
	 * @return the desired value if it exists in the properties object and can be parsed. None otherwise.
	 */
	def loadProperty(name: String, properties: Properties): Option[T] =
		getThisType(name, properties) match {
			case Some(v) => load(v.drop(1))
			case None => None
		}

	/**
	 * Converts the given value to a string representation
	 *
	 * @param value instance of an object to convert to a string
	 * @return a string representing the given value
	 */
	def write(value: T): String = value.toString


	/**
	 * Writes the target value to the properties file.
	 *
	 * @param name       to store the property as
	 * @param value      of the property to write
	 * @param properties file to write the property to
	 */
	def writeProperty(name: String, value: T, properties: Properties): Unit =
		properties.setProperty(name, prefix + write(value))


	/**
	 * Tests if a property with the specified name exists as an instance of this type.
	 * TODO: Fix potential problems arising from loading the wrong enum.
	 *
	 * @param name       name of the property to test existence of
	 * @param properties to test contain the desired property
	 * @return true if a property with the specified name matching type T exists in properties; false otherwise.
	 */
	def hasThisType(name: String, properties: Properties): Boolean =
		getThisType(name, properties).isDefined

	/**
	 * Attempts to get the value of the property with the given name; if it is of type T.
	 *
	 * @param name       of the property to get
	 * @param properties collection of properties to search
	 * @return the value mapped to the property with the desired name, if it is of type T. None otherwise.
	 */
	private def getThisType(name: String, properties: Properties): Option[String] =
		properties.getProperty(name) match {
			case p if p != null && p.nonEmpty && p.head == prefix => Some(p)
			case _ => None
		}
}


object EnumProperty {
	def apply[E <: Enumeration : ClassTag]: EnumProperty[E] = new EnumProperty[E]()

	def makeInstance[E <: Enumeration](clazz: Class[E]): EnumProperty[E] = {
		implicit val ct: ClassTag[E] = ClassTag(clazz)
		new EnumProperty[E]()
	}
}

/**
 * Class to handle Enumeration types
 *
 * @tparam E reference to the Enumeration type the class should handle
 */
class EnumProperty[E <: Enumeration](implicit ct: ClassTag[E]) extends TypedProperty[E#Value] {

	override def prefix: Char = 'e'

	/**
	 * Tries to find a value in the Enum E with the name value
	 *
	 * @param value name of the value to read.
	 * @return The value of the enum with the specified name. If one exists
	 */
	override def load(value: String): Option[E#Value] = {
		val mirror = runtimeMirror(getClass.getClassLoader)
		mirror.reflectModule(mirror.staticModule(ct.runtimeClass.getName.dropRight(1).replaceAll("\\$", ".")))
			.instance
			.asInstanceOf[E]
			.values
			.find(_.toString == value)
	}
}


/**
 * Class to handle String types
 */
object StringProperty extends TypedProperty[String] {
	override def prefix: Char = 'w'

	/**
	 * Some(identity) to conform to base load header
	 *
	 * @param value to wrap.
	 * @return Some(identity)
	 */
	override def load(value: String): Option[String] = Some(value)
}


/**
 * Class to handle Double types
 */
object DoubleProperty extends TypedProperty[Double] {
	/**
	 * d
	 *
	 * @return prefix representing Double
	 */
	override def prefix: Char = 'd'

	/**
	 * Attempts to parse the given value to an instance of type Double
	 *
	 * @param value to attempt to parse as a Double.
	 * @return an instance of a Double, If one can be created.
	 */
	override def load(value: String): Option[Double] = try {
		Some(value.toDouble)
	} catch {
		case _: NumberFormatException => None
	}
}

/**
 * Class to handle Float types
 */
object FloatProperty extends TypedProperty[Float] {
	/**
	 * f
	 *
	 * @return prefix representing Float
	 */
	override def prefix: Char = 'f'

	/**
	 * Attempts to parse the given value to an instance of type Float
	 *
	 * @param value to attempt to parse as a Float.
	 * @return an instance of a Float, If one can be created.
	 */
	override def load(value: String): Option[Float] = try {
		Some(value.toFloat)
	} catch {
		case _: NumberFormatException => None
	}
}


/**
 * Class to handle Long types
 */
object LongProperty extends TypedProperty[Long] {
	/**
	 * l
	 *
	 * @return prefix representing Long
	 */
	override def prefix: Char = 'l'

	/**
	 * Attempts to parse the given value to an instance of type Long
	 *
	 * @param value to attempt to parse as a Long.
	 * @return an instance of a Long, If one can be created.
	 */
	override def load(value: String): Option[Long] = try {
		Some(value.toLong)
	} catch {
		case _: NumberFormatException => None
	}
}

/**
 * Class to handle Int types
 */
object IntProperty extends TypedProperty[Int] {
	/**
	 * i
	 *
	 * @return prefix representing Int
	 */
	override def prefix: Char = 'i'

	/**
	 * Attempts to parse the given value to an instance of type Int
	 *
	 * @param value to attempt to parse as a Int.
	 * @return an instance of a Int, If one can be created.
	 */
	override def load(value: String): Option[Int] = try {
		Some(value.toInt)
	} catch {
		case _: NumberFormatException => None
	}
}

/**
 * Class to handle Char types
 */
object CharProperty extends TypedProperty[Char] {
	/**
	 * c
	 *
	 * @return prefix representing Char
	 */
	override def prefix: Char = 'c'

	/**
	 * Attempts to parse the given value to an instance of type Char
	 *
	 * @param value to attempt to parse as a Char.
	 * @return an instance of a Char, If one can be created.
	 */
	override def load(value: String): Option[Char] = if (value.length == 1) {
		Some(value(0))
	} else {
		None
	}
}


/**
 * Class to handle Short types
 */
object ShortProperty extends TypedProperty[Short] {
	/**
	 * s
	 *
	 * @return prefix representing Short
	 */
	override def prefix: Char = 's'

	/**
	 * Attempts to parse the given value to an instance of type Short
	 *
	 * @param value to attempt to parse as a Short.
	 * @return an instance of a Short, If one can be created.
	 */
	override def load(value: String): Option[Short] = try {
		Some(value.toShort)
	} catch {
		case _: NumberFormatException => None
	}
}

/**
 * Class to handle Byte types
 */
object ByteProperty extends TypedProperty[Byte] {
	/**
	 * b
	 *
	 * @return prefix representing Byte
	 */
	override def prefix: Char = 'b'

	/**
	 * Attempts to parse the given value to an instance of type Byte
	 *
	 * @param value to attempt to parse as a Byte.
	 * @return an instance of a Byte, If one can be created.
	 */
	override def load(value: String): Option[Byte] = try {
		Some(value.toByte)
	} catch {
		case _: NumberFormatException => None
	}
}

/**
 * Class to handle Boolean types
 */
object BooleanProperty extends TypedProperty[Boolean] {
	/**
	 * o
	 *
	 * @return prefix representing Boolean
	 */
	override def prefix: Char = 'o'

	/**
	 * Attempts to parse the given value to an instance of type Boolean
	 *
	 * @param value to attempt to parse as a Boolean.
	 * @return an instance of a Boolean, If one can be created.
	 */
	override def load(value: String): Option[Boolean] = try {
		Some(value.toBoolean)
	} catch {
		case _: IllegalArgumentException => None
	}
}