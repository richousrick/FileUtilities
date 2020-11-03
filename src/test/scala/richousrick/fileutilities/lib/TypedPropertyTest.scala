package richousrick.fileutilities.lib

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
