package richousrick.fileutilities.simpleversion

import java.nio.file.Files

import org.scalatest.funsuite.AnyFunSuite
import richousrick.fileutilities.lib.MockUtils

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

	test("Setup a linked backup file") {
		val (_, targetFile, backupDir) = MockUtils.generateMockFilesystemWin()
		val fileContents = Files.readAllLines(targetFile).asScala
		val backupInstance = backupDir.resolve("currentVersionInstance.current")

		// assert filesystem is empty before backup
		assert(Files.list(backupDir).count() == 0)
		SimpleVersion.initLink(backupDir, targetFile)

		// assert the file was successfully copied to the backup folder
		assert(Files.list(backupDir).count() == 1)
		assert(Files.exists(backupInstance))
		assert(Files.readAllLines(backupInstance).asScala == fileContents)

		// test link was created successfully
		assert(Files.isSymbolicLink(targetFile))
		assert(Files.readSymbolicLink(targetFile) == backupInstance)
	}

	test("Setup a linked backup directory") {
		var (fs, targetFile, backupDir) = MockUtils.generateMockFilesystemWin()

		// setup inner directory structure
		var fileContents = Map[String, Array[Byte]](
			"misc\\SomeFile" -> "This is another file with some stuff in it".getBytes(),
			"misc\\AnotherFile.bash" -> "echo \"The Third File.\nThe purpose of this is to emulate multiple nested files.\"".getBytes()
		)
		Files.createDirectories(targetFile.getParent.resolve("misc"))
		fileContents.foreach(f => Files.write(targetFile.getParent.resolve(f._1), f._2))

		fileContents += targetFile.getFileName.toString -> Files.readAllLines(targetFile).asScala.mkString("\n").getBytes()
		targetFile = targetFile.getParent
		val backupInstance = backupDir.resolve("currentVersionInstance.current")

		// assert filesystem is empty before backup
		assert(Files.list(backupDir).count() == 0)
		SimpleVersion.initLink(backupDir, targetFile)

		// assert the file was successfully copied to the backup folder
		fileContents.foreach(f => assert(Files.readAllLines(targetFile.resolve(f._1)).asScala.mkString("\n") == f
			._2
			.map(_.toChar)
			.mkString))

		// assert no other files exsist
		assert(Files
			.list(targetFile.resolve("misc"))
			.iterator()
			.asScala
			.map("misc\\" + _.getFileName.toString)
			.toSet == Set("misc\\SomeFile", "misc\\AnotherFile.bash"))
		fileContents -= "misc\\SomeFile"
		fileContents -= "misc\\AnotherFile.bash"
		fileContents += "misc" -> Array()
		assert(Files.list(targetFile).iterator().asScala.map(_.getFileName.toString).toSet == fileContents.keySet)


		// test link was created successfully
		assert(Files.isSymbolicLink(targetFile))
		assert(Files.readSymbolicLink(targetFile) == backupInstance)
	}
}
