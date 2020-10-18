package richousrick.fileutilities.simpleversion

import org.scalatest.funsuite.AnyFunSuite

import scala.jdk.CollectionConverters._

class SimpleVersionTest extends AnyFunSuite {

	test("Setup config should create correct properties") {
		def testParams(path: String, useLinks: Boolean) = {
			val prop = SimpleVersion.setupConfig(path, useLinks)
			assert(prop.propertyNames().asScala.toSet == Set("backupFile", "useLinks"))
			assert(prop.getProperty("backupFile") == path)
			assert(prop.getProperty("useLinks") == useLinks + "")
		}

		testParams("""D:\some\path\to the\File\target.txt""", useLinks = false)
		testParams("""D:\some\path\to the\File\target.txt""", useLinks = true)
		testParams("""D:\some\path\to the\File\""", useLinks = false)
		testParams("""D:\some\path\to the\File\""", useLinks = true)
	}
}
