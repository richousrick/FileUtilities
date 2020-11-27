package richousrick.fileutilities.propmanager

import scala.reflect.runtime.universe.{TypeRef, TypeTag, typeOf}

object TypeHandler {

  /** Handler supporting Bytes */
  val ByteHandler = new PrimitiveTypeHandler(ByteProperty)

  /** Handler supporting Shorts */
  val ShortHandler = new PrimitiveTypeHandler(ShortProperty)

  /** Handler supporting Integers */
  val IntHandler = new PrimitiveTypeHandler(IntProperty)

  /** Handler supporting Longs */
  val LongHandler = new PrimitiveTypeHandler(LongProperty)

  /** Handler supporting Floats */
  val FloatHandler = new PrimitiveTypeHandler(FloatProperty)

  /** Handler supporting Doubles */
  val DoubleHandler = new PrimitiveTypeHandler(DoubleProperty)

  /** Handler supporting Booleans */
  val BooleanHandler = new PrimitiveTypeHandler(BooleanProperty)

  /** Handler supporting Characters */
  val CharHandler = new PrimitiveTypeHandler(CharProperty)

  /** Handler supporting Strings */
  val StringHandler = new PrimitiveTypeHandler(StringProperty)


  /**
   * Handlers for primitive types and Strings
   */
  val baseHandlers: Set[TypeHandler[_]] = Set(ByteHandler,
    ShortHandler,
    IntHandler,
    LongHandler,
    FloatHandler,
    DoubleHandler,
    BooleanHandler,
    CharHandler,
    StringHandler)

  /**
   * Handlers to enable Enum types
   */
  val enumHandlers: Set[TypeHandler[_]] = Set(EnumTypeHandler, EnumValueHandler)

  /**
   * list of the default supported handlers
   */
  val defaultHandlers: Set[TypeHandler[_]] = baseHandlers ++ enumHandlers

  /**
   * Handlers that will be used as default by all functions
   */
  var globalHandlers: Set[TypeHandler[_]] = defaultHandlers


  /**
   * Gets an instance of [[richousrick.fileutilities.propmanager.TypedProperty TypedProperty]] that can handle a desired datatype.
   *
   * @param tt type tag of the desired datatype
   * @tparam T desired datatype
   * @return a [[richousrick.fileutilities.propmanager.TypedProperty TypedProperty]] that can handle data T, or None if T is an unsupported datatype
   */
  def resolveHandler[T](handlers: Set[TypeHandler[_]])(implicit tt: TypeTag[T]): Option[TypedProperty[T]] =
    handlers.collectFirst {
      case e if e.matchesType[T] => e.getInstance[T]
    }


  /**
   * Constructs a simple handler for a given type.
   * Note handlers are selected on a first found basis. Not best fit.
   * I.e. given handlers for types T, S &lt;: T, either handler may be selected for an object of type S, as both are valid.
   * However, the handler for S is not valid for data of type T.
   *
   * @param prefix_ character to prepend to the value when using typed mode.
   * @param write_  function to convert an object of type T to a string that can be written to the file
   * @param load_   function to attempt to load a value written using the write_ method.
   * @tparam T type of the data that this handler should be able to handle
   * @return a handler that supports type T
   */
  def buildHandler[T: TypeTag](prefix_ : Char,
                               load_ : String => Option[T],
                               write_ : T => String = (t: T) => t.toString): TypeHandler[T] = {
    new PrimitiveTypeHandler[T](new TypedProperty[T] {
      /**
       * Prefix for property name
       *
       * @return a prefix used to identify the property type
       */
      override def prefix: Char = prefix_

      /**
       * Attempts to parse the given value to an instance of type T
       *
       * @param value value to attempt to instantiate.
       * @return an instance of type T, If one can be created.
       */
      override def load(value: String): Option[T] = load_(value)

      override def write(value: T): String = write_(value)
    })
  }
}


/**
 * Wrapper for factory method to generate instances of TypedProperty[T]
 *
 * @param tt type tag for type T
 * @tparam T type of TypedProperty that this class can generate instances for
 */
abstract class TypeHandler[T](implicit tt: TypeTag[T]) {

  /**
   * Tests if TypedProperty[O] is a valid instance of TypedProperty[T].
   * This is used to determine if this instances getInstance method could be used to generate a valid TypedProperty
   *
   * @param to type tag for type O
   * @tparam O type to test this instance is able to handle
   * @return true if this class's getInstance method would be able to generate a valid TypedProperty[O]
   */
  def matchesType[O](implicit to: TypeTag[O]): Boolean = to.tpe <:< typeOf[T]

  /**
   * Factory method for TypedProperty[I]
   *
   * @param ti type tag for type I
   * @tparam I type of TypedProperty to generate an instance of. Note it is assumed that I < T of I = T.
   * @return an instance of TypedProperty that can handle the given type I
   */
  def getInstance[I](implicit ti: TypeTag[I]): TypedProperty[I]
}

/**
 * Class to handle statically implemented TypedProperties.
 *
 * @param handler typedProperty that this handler should support
 * @tparam T type of TypedProperty that this class can generate instances for
 */
class PrimitiveTypeHandler[T: TypeTag](private val handler: TypedProperty[T]) extends TypeHandler[T] {

  override def matchesType[M](implicit tm: _root_.scala.reflect.runtime.universe.TypeTag[M]): Boolean = typeOf[T] =:= tm.tpe

  override def getInstance[I](implicit ti: TypeTag[I]): TypedProperty[I] = handler.asInstanceOf[TypedProperty[I]]
}

/**
 * TypeHandler to generate instances from Enumeration values.
 * Note this will only work on specific enum instances, for instance RoundingMode.CEILING.type<br>
 * To generate instances from references to the Enum's type use [[richousrick.fileutilities.propmanager.EnumTypeHandler EnumTypeHandler]]
 */
object EnumValueHandler extends TypeHandler[Enumeration#Value] {
  override def getInstance[I](implicit tt: TypeTag[I]): TypedProperty[I] = EnumProperty.makeInstance(tt.mirror
    .runtimeClass(typeOf[I].widen.asInstanceOf[TypeRef].pre.typeSymbol.asClass)
    .asSubclass[Enumeration](classOf[Enumeration])).asInstanceOf[TypedProperty[I]]
}

/**
 * TypeHandler to generate instances from Enumeration types.
 * Note this will only work on enum types, for instance RoundingMode.type<br>
 * To generate instances from references to Enum's values use [[richousrick.fileutilities.propmanager.EnumValueHandler EnumValueTypeHandler]]
 */
object EnumTypeHandler extends TypeHandler[Enumeration] {
  override def getInstance[I](implicit ti: TypeTag[I]): TypedProperty[I] =
    EnumProperty.makeInstance(ti.mirror
      .runtimeClass(ti.tpe)
      .asSubclass[Enumeration](classOf[Enumeration])).asInstanceOf[TypedProperty[I]]
}