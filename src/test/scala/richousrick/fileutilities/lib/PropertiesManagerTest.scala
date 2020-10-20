package richousrick.fileutilities.lib

import java.nio.file.{FileSystem, Files, Path}
import java.util.Properties

import org.scalatest.BeforeAndAfterEach
import org.scalatest.funsuite.AnyFunSuite

class PropertiesManagerTest extends AnyFunSuite with BeforeAndAfterEach {
	var targetFile: Path = _
	var backupDir: Path = _
	var propLoc: Path = _
	var prop: Properties = _

	override def beforeEach() {
		// Gen mock file system containing target file
		val fs: FileSystem = MockUtils.generateMockFilesystemWin() match {
			case (a, b) =>
				targetFile = b
				a
			case _ => throw new UnsupportedOperationException()
		}
		backupDir = fs.getPath("""C:\data\backup""")
		propLoc = fs.getPath(backupDir + "\\versions.properties")

		// generate default config
		prop = new Properties()
		prop.setProperty("Lorem", "Ipsum")
		prop.setProperty("dolor", "sit ")
	}

	test("Create and read new config file") {
		assert(PropertiesManager.writeConfig(propLoc, prop))

		// test the config file was created, and when loaded is identical to the
		assert(Files.exists(propLoc))
		val loadedProps = PropertiesManager.readConfig(propLoc)
		assert(loadedProps.isDefined)
		assert(loadedProps.get == prop)
	}

	test("Overwrite and read config file") {
		assert(PropertiesManager.writeConfig(propLoc, prop))

		// assert current props match stored props
		assert(PropertiesManager.readConfig(propLoc).get == prop)

		// change property and verify differs from stored props
		prop.setProperty("otherProperty", "SomeValue")
		assert(PropertiesManager.readConfig(propLoc).get != prop)

		// rewrite and verify they match again
		PropertiesManager.writeConfig(propLoc, prop)
		assert(PropertiesManager.readConfig(propLoc).get == prop)
	}

	test("Try to load config from non existent file") {
		assert(PropertiesManager.readConfig(propLoc).isEmpty)
	}
}