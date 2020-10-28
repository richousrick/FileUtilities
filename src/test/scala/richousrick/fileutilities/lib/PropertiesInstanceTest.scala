package richousrick.fileutilities.lib

import org.scalatest.funsuite.AnyFunSuite
import richousrick.fileutilities.simplehotswap.SimpleHotswap.LinkType

import scala.math.BigDecimal.RoundingMode

class PropertiesInstanceTest extends AnyFunSuite {

	test("ReadEnum works properly") {
		val prop = new PropertiesInstance()
		assert(prop.readEnum[LinkType.type]("Copy").contains(LinkType.Copy))
		assert(prop.readEnum[LinkType.type]("Hard").contains(LinkType.Hard))
		assert(prop.readEnum[LinkType.type]("Symbolic").contains(LinkType.Symbolic))
		assert(prop.readEnum[LinkType.type]("Yolo").isEmpty)
		assert(prop.readEnum[RoundingMode.type]("CEILING").contains(RoundingMode.CEILING))
	}
}
