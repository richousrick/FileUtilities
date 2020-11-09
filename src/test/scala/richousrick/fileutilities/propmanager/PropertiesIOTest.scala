package richousrick.fileutilities.propmanager

import java.nio.file.{FileSystem, Files, NoSuchFileException, Path}
import java.util.Properties

import org.scalatest.BeforeAndAfterEach
import org.scalatest.funsuite.AnyFunSuite
import richousrick.fileutilities.lib.MockUtils

class PropertiesIOTest extends AnyFunSuite with BeforeAndAfterEach {
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
		assert(PropertiesIO.writeConfigSwallow(propLoc, prop))

		// test the config file was created, and when loaded is identical to the
		assert(Files.exists(propLoc))
		val loadedProps = PropertiesIO.readConfigSwallow(propLoc)
		assert(loadedProps.isDefined)
		assert(loadedProps.get == prop)
	}

	test("Overwrite and read config file") {
		assert(PropertiesIO.writeConfigSwallow(propLoc, prop))

		// assert current props match stored props
		assert(PropertiesIO.readConfigSwallow(propLoc).get == prop)

		// change property and verify differs from stored props
		prop.setProperty("otherProperty", "SomeValue")
		assert(PropertiesIO.readConfigSwallow(propLoc).get != prop)

		// rewrite and verify they match again
		PropertiesIO.writeConfigSwallow(propLoc, prop)
		assert(PropertiesIO.readConfigSwallow(propLoc).get == prop)
	}

	test("Try to load config from non existent file") {
		assert(PropertiesIO.readConfigSwallow(propLoc).isEmpty)
	}

	test("Loading config into existing properties instance") {
		// write properties to disk
		assert(PropertiesIO.writeConfigSwallow(propLoc, prop))

		// create new properties file with properties
		val newProp = new Properties()
		newProp.setProperty("otherProperty", "SomeValue")
		newProp.setProperty("dolor", "stand")
		assert(newProp.getProperty("otherProperty") == "SomeValue")
		assert(newProp.getProperty("dolor") == "stand")

		// load properties and test old properties exist
		assert(PropertiesIO.readConfigSwallow(propLoc, newProp).isDefined)
		// test property not deleted
		assert(newProp.getProperty("otherProperty") == "SomeValue")
		// test property overwritten on load

		assert(newProp.getProperty("dolor") == "sit ")
		// test non existant property was loaded
		assert(newProp.getProperty("Lorem") == "Ipsum")
	}

	test("Read config throws exception on missing file") {
		assertThrows[NoSuchFileException](PropertiesIO.readConfig(propLoc.resolve("missingDIr"), prop))
	}
}