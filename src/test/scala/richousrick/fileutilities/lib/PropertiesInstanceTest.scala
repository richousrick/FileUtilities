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
}
