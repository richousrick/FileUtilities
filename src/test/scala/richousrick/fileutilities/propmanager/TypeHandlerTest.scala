package richousrick.fileutilities.propmanager

import java.nio.file.Path

import org.scalatest.funsuite.AnyFunSuite
import richousrick.fileutilities.propmanager.TypeHandler._
import richousrick.fileutilities.simplehotswap.SimpleHotswap.LinkType

import scala.math.BigDecimal.RoundingMode
import scala.reflect.runtime.universe.TypeTag

class TypeHandlerTest extends AnyFunSuite {

  test("resolveHandler resolves the correct primitive types") {
    tryTest[String](defaultHandlers)
    tryTest[Char](defaultHandlers)
    tryTest[Byte](defaultHandlers)
    tryTest[Short](defaultHandlers)
    tryTest[Int](defaultHandlers)
    tryTest[Long](defaultHandlers)
    tryTest[Float](defaultHandlers)
    tryTest[Double](defaultHandlers)
  }

  test("resolveHandler resolves the correct primitive types with generated handlers") {
    val generatedHandlers: Set[TypeHandler[_]] = Set(
      buildHandler('b', _.toByteOption),
      buildHandler('s', _.toShortOption),
      buildHandler('i', _.toIntOption),
      buildHandler('l', _.toLongOption),
      buildHandler('f', _.toFloatOption),
      buildHandler('d', _.toDoubleOption),
      buildHandler('o', _.toBooleanOption),
      buildHandler('c', (s: String) => if (s.length == 1) {
        Some(s(0))
      } else {
        None
      }),
      buildHandler[String]('w', Some(_))
    )
    tryTest[String](generatedHandlers)
    tryTest[Char](generatedHandlers)
    tryTest[Byte](generatedHandlers)
    tryTest[Short](generatedHandlers)
    tryTest[Int](generatedHandlers)
    tryTest[Long](generatedHandlers)
    tryTest[Float](generatedHandlers)
    tryTest[Double](generatedHandlers)
  }

  test("resolverHandler resolves enums from values") {
    val rm = resolveHandler[RoundingMode.CEILING.type](defaultHandlers)
    assert(rm.isDefined)
    assert(rm.get.isInstanceOf[EnumProperty[RoundingMode.type]])

    val lt = resolveHandler[LinkType.Copy.type](defaultHandlers)
    assert(lt.isDefined)
    assert(lt.get.isInstanceOf[EnumProperty[LinkType.type]])
  }

  test("resolverHandler resolves enum types") {
    val rm = resolveHandler[RoundingMode.type](defaultHandlers)
    assert(rm.isDefined)
    assert(rm.get.isInstanceOf[EnumProperty[RoundingMode.type]])

    val lt = resolveHandler[LinkType.type](defaultHandlers)
    assert(lt.isDefined)
    assert(lt.get.isInstanceOf[EnumProperty[LinkType.type]])
  }

  test("resolverHandler fails safe") {
    assert(resolveHandler[Path](defaultHandlers).isEmpty)
    assert(resolveHandler[List[String]](defaultHandlers).isEmpty)
  }

  def tryTest[T: TypeTag](handlers: Set[TypeHandler[_]]): Unit = {
    assert(resolveHandler[T](handlers).isDefined)
  }
}
