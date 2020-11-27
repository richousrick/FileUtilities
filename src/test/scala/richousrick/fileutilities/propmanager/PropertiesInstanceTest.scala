package richousrick.fileutilities.propmanager

import java.nio.file.{Path, Paths}

import org.scalatest.funsuite.AnyFunSuite
import richousrick.fileutilities.lib.MockUtils
import richousrick.fileutilities.simplehotswap.LinkType

import scala.math.BigDecimal.RoundingMode
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag
import scala.util.{Random, Try}

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

	test("Unsupported handler throws exception") {
		assertThrows[UnsupportedOperationException](properties.setProperty[Path]("pathVar", Paths.get("c:/")))
		assertThrows[UnsupportedOperationException](properties.getProperty[Path]("pathVar"))
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

	test("Properties read/write") {
		val fs = MockUtils.generateMockFilesystemWin()._1
		val propLoc = fs.getPath("SomeFile.prop")
		assert(!properties.load(propLoc))
		assert(properties.write(propLoc))
		assert(properties.load(propLoc))
	}

	test("Properties specific type handling") {
		val (fs, somePath) = MockUtils.generateMockFilesystemWin()
		val p2 = PropertiesInstance(Set[TypeHandler[_]](TypeHandler.buildHandler('P',
			(s: String) => Try(fs.getPath(s)).toOption)))

		assertThrows[UnsupportedOperationException](properties.setProperty("myPath", somePath))
		assert(p2.setProperty("myPath", somePath).isEmpty)

		assertThrows[UnsupportedOperationException](properties.getProperty[Path]("myPath"))
		assert({
			val loadedPath = p2.getProperty[Path]("myPath"); loadedPath.nonEmpty && loadedPath.get == somePath
		})
	}

	test("Adding global type handlers") {
		val (fs, somePath) = MockUtils.generateMockFilesystemWin()

		val pathHandler = TypeHandler.buildHandler('P', (s: String) => Try(fs.getPath(s)).toOption)

		assertThrows[UnsupportedOperationException](properties.setProperty("myPath", somePath))
		assertThrows[UnsupportedOperationException](properties.getProperty[Path]("myPath"))

		TypeHandler.globalHandlers += pathHandler

		assert(properties.setProperty("myPath", somePath).isEmpty)
		assert({
			val loadedPath = properties.getProperty[Path]("myPath")
			loadedPath.nonEmpty && loadedPath.get == somePath
		})

		// cleanUp
		TypeHandler.globalHandlers -= pathHandler
	}

	test("Replacing type handlers") {
		val add10 = TypeHandler.buildHandler('i', _.toIntOption match {
			case Some(i) => Some(i + 10)
			case None => None
		})

		val p2 = new PropertiesInstance(false, Set(add10), Set(TypeHandler.IntHandler))

		assert(properties.setProperty("FortyTwo", 42).isEmpty)
		assert(p2.setProperty("FortyTwo", 42).isEmpty)

		assert(properties.getProperty[Int]("FortyTwo").contains(42))
		assert(p2.getProperty[Int]("FortyTwo").contains(52))
	}

	test("Ignore global handlers") {
		val add10 = TypeHandler.buildHandler('i', _.toIntOption match {
			case Some(i) => Some(i + 10)
			case None => None
		})

		val p1 = new PropertiesInstance(typed = false, ignoreGlobalHandlers = true)

		val p2 = new PropertiesInstance(false, Set(add10), ignoreGlobalHandlers = true)

		assertThrows[UnsupportedOperationException](p1.setProperty("FortyTwo", 42))
		assert(p2.setProperty("FortyTwo", 42).isEmpty)

		assertThrows[UnsupportedOperationException](p1.getProperty[Int]("FortyTwo"))
		assert(p2.getProperty[Int]("FortyTwo").contains(52))
	}
}
