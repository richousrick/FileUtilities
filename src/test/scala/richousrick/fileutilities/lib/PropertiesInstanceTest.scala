package richousrick.fileutilities.lib

import org.scalatest.funsuite.AnyFunSuite
import richousrick.fileutilities.simplehotswap.SimpleHotswap.LinkType

import scala.math.BigDecimal.RoundingMode

class PropertiesInstanceTest extends AnyFunSuite {

	test("ReadEnum works properly") {
		assert(PropertiesInstance.readEnum[LinkType.type]("Copy").contains(LinkType.Copy))
		assert(PropertiesInstance.readEnum[LinkType.type]("Hard").contains(LinkType.Hard))
		assert(PropertiesInstance.readEnum[LinkType.type]("Symbolic").contains(LinkType.Symbolic))
		assert(PropertiesInstance.readEnum[LinkType.type]("Yolo").isEmpty)
		assert(PropertiesInstance.readEnum[RoundingMode.type]("CEILING").contains(RoundingMode.CEILING))
	}

	test("ReadVal works for Booleans") {
		assert(PropertiesInstance.readVal[Boolean]("true").contains(true))
		assert(PropertiesInstance.readVal[Boolean]("false").contains(false))
		assert(PropertiesInstance.readVal[Boolean]("Hello").isEmpty)
	}

	test("ReadVal works for Bytes") {
		assert(PropertiesInstance.readVal[Byte]("0").contains(0.toByte))
		assert(PropertiesInstance.readVal[Byte]("120").contains(120.toByte))
		assert(PropertiesInstance.readVal[Byte]("512").isEmpty)
		assert(PropertiesInstance.readVal[Byte]("how").isEmpty)
	}

	test("ReadVal works for Shorts") {
		assert(PropertiesInstance.readVal[Short]("0").contains(0.toShort))
		assert(PropertiesInstance.readVal[Short](Short.MinValue.toString).contains(Short.MinValue))
		assert(PropertiesInstance.readVal[Short](Short.MaxValue.toString).contains(Short.MaxValue))
		assert(PropertiesInstance.readVal[Short](Int.MaxValue.toString).isEmpty)
		assert(PropertiesInstance.readVal[Short]("are").isEmpty)
	}

	test("ReadVal works for Ints") {
		assert(PropertiesInstance.readVal[Int]("0").contains(0.toInt))
		assert(PropertiesInstance.readVal[Int](Int.MinValue.toString).contains(Int.MinValue))
		assert(PropertiesInstance.readVal[Int](Int.MaxValue.toString).contains(Int.MaxValue))
		assert(PropertiesInstance.readVal[Int](Long.MaxValue.toString).isEmpty)
		assert(PropertiesInstance.readVal[Int]("you").isEmpty)
	}

	test("ReadVal works for Longs") {
		assert(PropertiesInstance.readVal[Long]("0").contains(0.toLong))
		assert(PropertiesInstance.readVal[Long](Long.MinValue.toString).contains(Long.MinValue))
		assert(PropertiesInstance.readVal[Long](Long.MaxValue.toString).contains(Long.MaxValue))
		assert(PropertiesInstance.readVal[Long]("1" + Long.MaxValue).isEmpty)
		assert(PropertiesInstance.readVal[Long]("doing").isEmpty)
	}

	test("ReadVal works for Floats") {
		assert(PropertiesInstance.readVal[Float]("0").contains(0.toFloat))
		assert(PropertiesInstance.readVal[Float](Float.MinValue.toString).contains(Float.MinValue))
		assert(PropertiesInstance.readVal[Float](Float.MaxValue.toString).contains(Float.MaxValue))
		assert(PropertiesInstance.readVal[Float]("1" + Float.MaxValue).contains(Float.PositiveInfinity))
		assert(PropertiesInstance.readVal[Float]("today").isEmpty)
	}

	test("ReadVal works for Doubles") {
		assert(PropertiesInstance.readVal[Double]("0").contains(0.toDouble))
		assert(PropertiesInstance.readVal[Double](Double.MinValue.toString).contains(Double.MinValue))
		assert(PropertiesInstance.readVal[Double](Double.MaxValue.toString).contains(Double.MaxValue))
		assert(PropertiesInstance.readVal[Double]("1" + Double.MaxValue).contains(Double.PositiveInfinity))
		assert(PropertiesInstance.readVal[Double]("well").isEmpty)
	}

	test("ReadVal works for Chars") {
		assert(PropertiesInstance.readVal[Char]("i").contains('i'))
		assert(PropertiesInstance.readVal[Char]("hope").isEmpty)
		assert(PropertiesInstance.readVal[Char]("!").contains('!'))
	}
}
