package richousrick.fileutilities.propmanager

import java.nio.file.Path

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.{PrivateMethodTester, Suites}
import richousrick.fileutilities.simplehotswap.SimpleHotswap.LinkType

import scala.math.BigDecimal.RoundingMode
import scala.reflect.runtime.universe.TypeTag

class PropertiesInstanceTestSuite extends Suites {
	new PropertiesInstanceResolverTest
}


class PropertiesInstanceResolverTest extends AnyFunSuite with PrivateMethodTester {

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
		val rm = resolveHandler[RoundingMode.CEILING.type]
		assert(rm.isDefined)
		assert(rm.get.isInstanceOf[EnumProperty[RoundingMode.type]])

		val lt = resolveHandler[LinkType.Copy.type]
		assert(lt.isDefined)
		assert(lt.get.isInstanceOf[EnumProperty[LinkType.type]])
	}

	test("resolverHandler resolves enum types") {
		val rm = resolveHandler[RoundingMode.type]
		assert(rm.isDefined)
		assert(rm.get.isInstanceOf[EnumProperty[RoundingMode.type]])

		val lt = resolveHandler[LinkType.type]
		assert(lt.isDefined)
		assert(lt.get.isInstanceOf[EnumProperty[LinkType.type]])
	}

	test("resolverHandler fails safe") {
		assert(resolveHandler[Path].isEmpty)
		assert(resolveHandler[List[String]].isEmpty)
	}

	def tryTest[T: TypeTag](expected: TypedProperty[_]): Unit = {
		assert(resolveHandler[T].contains(expected))
	}

	def resolveHandler[T: TypeTag]: Option[TypedProperty[T]] = {
		val handle = PrivateMethod[Option[TypedProperty[T]]](Symbol("resolveHandler"))
		PropertiesInstance invokePrivate handle(implicitly[TypeTag[T]])
	}
}

