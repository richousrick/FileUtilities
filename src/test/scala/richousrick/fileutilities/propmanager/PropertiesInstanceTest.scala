package richousrick.fileutilities.propmanager

import org.scalatest.funsuite.AnyFunSuite
import richousrick.fileutilities.simplehotswap.SimpleHotswap.LinkType

import scala.math.BigDecimal.RoundingMode
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag
import scala.util.Random

class PropertiesInstanceTest extends AnyFunSuite {
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
		if (properties.containsKey(name)) {
			assert(properties.setProperty[T](name, value).isDefined)
		} else {
			assert(properties.getProperty[T](name).isEmpty)
			assert(properties.setProperty[T](name, value).isEmpty)
		}
		assert(properties.getProperty[T](name).contains(value))
	}
}