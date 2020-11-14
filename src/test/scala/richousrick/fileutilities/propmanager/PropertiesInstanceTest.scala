package richousrick.fileutilities.propmanager

import java.nio.file.Path

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.{PrivateMethodTester, Suites}
import richousrick.fileutilities.simplehotswap.SimpleHotswap.LinkType

import scala.math.BigDecimal.RoundingMode
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag
import scala.util.Random

class PropertiesInstanceTestSuite extends Suites {
	new PropertiesInstanceResolverTest
	new PropertiesInstanceResolverCompanionTest
}

class PropertiesInstanceResolverTest extends AnyFunSuite {
	val properties: PropertiesInstance = PropertiesInstance()

	test("Enum Read/Write") {
		readWriteTest("Cpy", LinkType.Copy)
		readWriteTest("Hrd", LinkType.Hard)
		readWriteTest("Sym", LinkType.Symbolic)
		readWriteTest("CEIL", RoundingMode.CEILING)
	}

	test("Boolean Read/Write") {
		readWriteTest("yes", true)
		readWriteTest("no", false)
	}

	test("Byte Read/Write") {
		readWriteTest("zero", 0.toByte)
		readWriteTest("pos", 120.toByte)
		readWriteTest("neg", -120.toByte)
	}

	test("Short Read/Write") {
		readWriteTest("zero", 0.toShort)
		readWriteTest("pos", Short.MaxValue)
		readWriteTest("neg", Short.MinValue)
	}

	test("Int Read/Write") {
		readWriteTest("zero", 0)
		readWriteTest("pos", Int.MaxValue)
		readWriteTest("neg", Int.MinValue)
	}

	test("Long Read/Write") {
		readWriteTest("zero", 0L)
		readWriteTest("pos", Long.MaxValue)
		readWriteTest("neg", Long.MinValue)
	}

	test("Float Read/Write") {
		readWriteTest("zero", 0f)
		readWriteTest("pos", Float.MaxValue)
		readWriteTest("neg", Float.MinValue)
		readWriteTest("pinf", Float.PositiveInfinity)
		readWriteTest("ninf", Float.NegativeInfinity)
	}

	test("Double Read/Write") {
		readWriteTest("zero", 0d)
		readWriteTest("pos", Double.MaxValue)
		readWriteTest("neg", Double.MinValue)
		readWriteTest("pinf", Double.PositiveInfinity)
		readWriteTest("ninf", Double.NegativeInfinity)
	}

	test("Char Read/Write") {
		readWriteTest("i", 'i')
		readWriteTest("!", '!')
		readWriteTest("tab", '\t')
	}

	test("String Read/Write") {
		Random.setSeed(3)
		readWriteTest("generated", Random.nextString(1024 * 512))
		readWriteTest("empty", "")
	}

	def readWriteTest[T](name: String, value: T)(implicit tt: TypeTag[T], ct: ClassTag[T]): Unit = {
		assert(properties.setProperty[T](name, value))
		assert(properties.getProperty[T](name).contains(value))
	}
}

class PropertiesInstanceResolverCompanionTest extends AnyFunSuite with PrivateMethodTester {

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

