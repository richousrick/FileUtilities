package richousrick.fileutilities.lib

import java.util.Properties

import org.scalatest.Suites
import org.scalatest.funsuite.AnyFunSuite
import richousrick.fileutilities.simplehotswap.SimpleHotswap.LinkType

import scala.math.BigDecimal.RoundingMode
import scala.util.Random

class TypedPropertyTest extends AnyFunSuite {

	test("Load works for Enums") {
		assert(EnumProperty[LinkType.type].load("Copy").contains(LinkType.Copy))
		assert(EnumProperty[LinkType.type].load("Hard").contains(LinkType.Hard))
		assert(EnumProperty[LinkType.type].load("Symbolic").contains(LinkType.Symbolic))
		assert(EnumProperty[LinkType.type].load("Yolo").isEmpty)
		assert(EnumProperty[RoundingMode.type].load("CEILING").contains(RoundingMode.CEILING))
	}

	test("Load works for Booleans") {
		assert(BooleanProperty.load("true").contains(true))
		assert(BooleanProperty.load("false").contains(false))
		assert(BooleanProperty.load("Hello").isEmpty)
	}

	test("Load works for Bytes") {
		assert(ByteProperty.load("0").contains(0.toByte))
		assert(ByteProperty.load("120").contains(120.toByte))
		assert(ByteProperty.load("512").isEmpty)
		assert(ByteProperty.load("how").isEmpty)
	}

	test("Load works for Shorts") {
		assert(ShortProperty.load("0").contains(0.toShort))
		assert(ShortProperty.load(Short.MinValue.toString).contains(Short.MinValue))
		assert(ShortProperty.load(Short.MaxValue.toString).contains(Short.MaxValue))
		assert(ShortProperty.load(Int.MaxValue.toString).isEmpty)
		assert(ShortProperty.load("are").isEmpty)
	}

	test("Load works for Ints") {
		assert(IntProperty.load("0").contains(0.toInt))
		assert(IntProperty.load(Int.MinValue.toString).contains(Int.MinValue))
		assert(IntProperty.load(Int.MaxValue.toString).contains(Int.MaxValue))
		assert(IntProperty.load(Long.MaxValue.toString).isEmpty)
		assert(IntProperty.load("you").isEmpty)
	}

	test("Load works for Longs") {
		assert(LongProperty.load("0").contains(0.toLong))
		assert(LongProperty.load(Long.MinValue.toString).contains(Long.MinValue))
		assert(LongProperty.load(Long.MaxValue.toString).contains(Long.MaxValue))
		assert(LongProperty.load("1" + Long.MaxValue).isEmpty)
		assert(LongProperty.load("doing").isEmpty)
	}

	test("Load works for Floats") {
		assert(FloatProperty.load("0").contains(0.toFloat))
		assert(FloatProperty.load(Float.MinValue.toString).contains(Float.MinValue))
		assert(FloatProperty.load(Float.MaxValue.toString).contains(Float.MaxValue))
		assert(FloatProperty.load("1" + Float.MaxValue).contains(Float.PositiveInfinity))
		assert(FloatProperty.load("today").isEmpty)
	}

	test("Load works for Doubles") {
		assert(DoubleProperty.load("0").contains(0.toDouble))
		assert(DoubleProperty.load(Double.MinValue.toString).contains(Double.MinValue))
		assert(DoubleProperty.load(Double.MaxValue.toString).contains(Double.MaxValue))
		assert(DoubleProperty.load("1" + Double.MaxValue).contains(Double.PositiveInfinity))
		assert(DoubleProperty.load("well").isEmpty)
	}

	test("Load works for Chars") {
		assert(CharProperty.load("i").contains('i'))
		assert(CharProperty.load("hope").isEmpty)
		assert(CharProperty.load("!").contains('!'))
	}

	test("Load works for Strings") {
		Random.setSeed(2)
		val str = Random.nextString(1024 * 512)
		assert(StringProperty.load(str).contains(str))

		assert(StringProperty.load("").contains(""))
	}
}

class TypedPropertyReadWriteTest extends AnyFunSuite {
	val properties = new Properties()


	test("Enum Read/Write") {
		readWriteTest(EnumProperty[LinkType.type], "c", LinkType.Copy)
		readWriteTest(EnumProperty[LinkType.type], "h", LinkType.Hard)
		readWriteTest(EnumProperty[LinkType.type], "s", LinkType.Symbolic)
		readWriteTest(EnumProperty[RoundingMode.type], "r", RoundingMode.CEILING)
	}

	test("Boolean Read/Write") {
		readWriteTest(BooleanProperty, "t", true)
		readWriteTest(BooleanProperty, "f", false)
	}

	test("Byte Read/Write") {
		readWriteTest(ByteProperty, "zero", 0.toByte)
		readWriteTest(ByteProperty, "pos", 120.toByte)
		readWriteTest(ByteProperty, "neg", -120.toByte)
	}

	test("Short Read/Write") {
		readWriteTest(ShortProperty, "zero", 0.toShort)
		readWriteTest(ShortProperty, "pos", Short.MaxValue)
		readWriteTest(ShortProperty, "neg", Short.MinValue)
	}

	test("Int Read/Write") {
		readWriteTest(IntProperty, "zero", 0)
		readWriteTest(IntProperty, "pos", Int.MaxValue)
		readWriteTest(IntProperty, "neg", Int.MinValue)
	}

	test("Long Read/Write") {
		readWriteTest(LongProperty, "zero", 0L)
		readWriteTest(LongProperty, "pos", Long.MaxValue)
		readWriteTest(LongProperty, "neg", Long.MinValue)
	}

	test("Float Read/Write") {
		readWriteTest(FloatProperty, "zero", 0f)
		readWriteTest(FloatProperty, "pos", Float.MaxValue)
		readWriteTest(FloatProperty, "neg", Float.MinValue)
		readWriteTest(FloatProperty, "pinf", Float.PositiveInfinity)
		readWriteTest(FloatProperty, "ninf", Float.NegativeInfinity)
	}

	test("Double Read/Write") {
		readWriteTest(DoubleProperty, "zero", 0d)
		readWriteTest(DoubleProperty, "pos", Double.MaxValue)
		readWriteTest(DoubleProperty, "neg", Double.MinValue)
		readWriteTest(DoubleProperty, "pinf", Double.PositiveInfinity)
		readWriteTest(DoubleProperty, "ninf", Double.NegativeInfinity)
	}

	test("Char Read/Write") {
		readWriteTest(CharProperty, "i", 'i')
		readWriteTest(CharProperty, "!", '!')
		readWriteTest(CharProperty, "tab", '\t')
	}

	test("String Read/Write") {
		Random.setSeed(3)
		readWriteTest(StringProperty, "generated", Random.nextString(1024 * 512))
		readWriteTest(StringProperty, "empty", "")
	}

	test("Wrong type test") {
		IntProperty.writeProperty("nought", 0, properties)
		assert(properties.getProperty("nought") == "i0")
		assert(ByteProperty.loadProperty("nought", properties).isEmpty)
		assert(IntProperty.loadProperty("nought", properties).contains(0))

		LongProperty.writeProperty("BigLong", Long.MaxValue, properties)
		assert(IntProperty.loadProperty("BigLong", properties).isEmpty)
		assert(LongProperty.loadProperty("BigLong", properties).contains(Long.MaxValue))

		EnumProperty[LinkType.type].writeProperty("Copy Type", LinkType.Copy, properties)
		assert(StringProperty.loadProperty("Copy Type", properties).isEmpty)
		assert(EnumProperty[LinkType.type].loadProperty("Copy Type", properties).contains(LinkType.Copy))
	}

	/**
	 * Writes the given value to the properties file, prefixing the name.
	 * Then reads the value back and tests if it is equal to the initial value
	 *
	 * @param tp    TypedProperty instance to read/write the property.
	 * @param name  unprefixed name to write the value as
	 * @param value to write / read to the properties file
	 * @tparam T base type of the value being written
	 */
	def readWriteTest[T](tp: TypedProperty[T], name: String, value: T): Unit = {
		tp.writeProperty(tp.prefix + name, value, properties)
		assert(tp.loadProperty(tp.prefix + name, properties).contains(value))
	}
}

class TypedPropertySuite extends Suites {
	new TypedPropertyTest; new TypedPropertyReadWriteTest
}
