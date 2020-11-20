package richousrick.fileutilities.propmanager

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe.{TypeRef, TypeTag, typeOf}

object TypeHandler {

  /**
   * Handlers for primitive types and Strings
   */
  val baseHandlers: Set[TypeHandler[_]] = Set(
    new PrimitiveTypeHandler(ByteProperty),
    new PrimitiveTypeHandler(ShortProperty),
    new PrimitiveTypeHandler(IntProperty),
    new PrimitiveTypeHandler(LongProperty),
    new PrimitiveTypeHandler(FloatProperty),
    new PrimitiveTypeHandler(DoubleProperty),
    new PrimitiveTypeHandler(BooleanProperty),
    new PrimitiveTypeHandler(CharProperty),
    new PrimitiveTypeHandler(StringProperty)
  )

  /**
   * Handlers to enable Enum types
   */
  val enumHandlers: Set[TypeHandler[_]] = Set(new EnumTypeHandler(), new EnumValueTypeHandler())

  /**
   * list of the default handlers
   */
  val defaultHandlers: Set[TypeHandler[_]] = baseHandlers ++ enumHandlers

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
class EnumValueTypeHandler extends TypeHandler[Enumeration#Value] {
  override def getInstance[I](implicit tt: universe.TypeTag[I]): TypedProperty[I] = EnumProperty.makeInstance(tt.mirror
    .runtimeClass(typeOf[I].widen.asInstanceOf[TypeRef].pre.typeSymbol.asClass)
    .asSubclass[Enumeration](classOf[Enumeration])).asInstanceOf[TypedProperty[I]]
}

/**
 * TypeHandler to generate instances from Enumeration types.
 * Note this will only work on enum types, for instance RoundingMode.type<br>
 * To generate instances from references to Enum's values use [[richousrick.fileutilities.propmanager.EnumValueTypeHandler EnumValueTypeHandler]]
 */
class EnumTypeHandler extends TypeHandler[Enumeration] {
  override def getInstance[I](implicit ti: universe.TypeTag[I]): TypedProperty[I] =
    EnumProperty.makeInstance(ti.mirror
      .runtimeClass(ti.tpe)
      .asSubclass[Enumeration](classOf[Enumeration])).asInstanceOf[TypedProperty[I]]
}