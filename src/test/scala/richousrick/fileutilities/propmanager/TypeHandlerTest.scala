package richousrick.fileutilities.propmanager

import java.nio.file.Path

import org.scalatest.funsuite.AnyFunSuite
import richousrick.fileutilities.propmanager.TypeHandler._
import richousrick.fileutilities.simplehotswap.SimpleHotswap.LinkType

import scala.math.BigDecimal.RoundingMode
import scala.reflect.runtime.universe.TypeTag

class TypeHandlerTest extends AnyFunSuite {

  test("resolveHandler resolves the correct primitive types") {
    tryTest[String](StringProperty)
    tryTest[Char](CharProperty)
    tryTest[Byte](ByteProperty)
    tryTest[Short](ShortProperty)
    tryTest[Int](IntProperty)
    tryTest[Long](LongProperty)
    tryTest[Float](FloatProperty)
    tryTest[Double](DoubleProperty)
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

  def tryTest[T: TypeTag](expected: TypedProperty[_]): Unit = {
    assert(resolveHandler[T](defaultHandlers).contains(expected))
    assert(resolveHandler[T](baseHandlers).contains(expected))
  }
}
