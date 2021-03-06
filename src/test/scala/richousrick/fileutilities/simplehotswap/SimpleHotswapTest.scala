package richousrick.fileutilities.simplehotswap

import java.nio.channels.{Channels, FileChannel}
import java.nio.file.{Files, StandardOpenOption}

import org.scalatest.funsuite.AnyFunSuite
import richousrick.fileutilities.lib.MockUtils

import scala.jdk.CollectionConverters._
import scala.util.Using

class SimpleHotswapTest extends AnyFunSuite {

	test("Setup a symbolic linked backup file") {
		val (fs, targetFile) = MockUtils.generateMockFilesystemWin()
		val backupDir = fs.getPath("""C:\data\backup""")
		val fileContents = Files.readAllLines(targetFile).asScala
		val backupInstance = backupDir.resolve("currentVersionInstance.current")

		// create backup folder
		Files.createDirectories(backupDir)

		// assert filesystem is empty before backup
		assert(Files.list(backupDir).count() == 0)

		assert(SimpleHotswap.setupInstance(backupDir, targetFile, LinkType.Symbolic).isDefined)

		// assert the file was successfully copied to the backup folder
		assert(Files.list(backupDir).count() == 2)
		assert(Files.exists(backupInstance))
		assert(Files.exists(backupDir.resolve("hotswap.properties")))
		assert(Files.readAllLines(backupInstance).asScala == fileContents)

		// test link was created successfully
		assert(Files.isSymbolicLink(targetFile))
		assert(Files.readSymbolicLink(targetFile) == backupInstance)
	}

	test("Setup a symbolic linked backup directory") {
		val (fs, targetFile, filesToBackup) = MockUtils.generateMockFilesystemWinDir()
		val backupDir = fs.getPath("""C:\data\backup""")
		val backupInstance = backupDir.resolve("currentVersionInstance.current")


		// create backup folder
		Files.createDirectories(backupDir)

		// assert filesystem is empty before backup
		assert(Files.list(backupDir).count() == 0)
		assert(SimpleHotswap.setupInstance(backupDir, targetFile, LinkType.Symbolic).isDefined)

		// assert the file was successfully copied to the backup folder
		// Convert both to string instead of for easier readability in print
		filesToBackup.foreach(f => assert(Files.readAllBytes(
			backupInstance.resolve(f._1)).map(_.toChar).mkString ==
			f._2.map(_.toChar).mkString))

		// assert no other files exsist
		assert(Files.walk(backupInstance).map(_.getFileName.toString).toArray.toSet
			== filesToBackup.keySet.map(_.getFileName.toString) ++ Set("currentVersionInstance.current", "misc"))

		// test link was created successfully
		assert(Files.isSymbolicLink(targetFile))
		assert(Files.readSymbolicLink(targetFile) == backupInstance)
	}

	test("Setup a hard linked backup file") {
		val (fs, targetFile) = MockUtils.generateMockFilesystemWin()
		val backupDir = fs.getPath("""C:\data\backup""")
		val fileContents = Files.readAllLines(targetFile).asScala
		val backupInstance = backupDir.resolve("currentVersionInstance.current")

		// create backup folder
		Files.createDirectories(backupDir)

		// assert filesystem is empty before backup
		assert(Files.list(backupDir).count() == 0)
		assert(SimpleHotswap.setupInstance(backupDir, targetFile, LinkType.Hard).isDefined)

		// assert the file was successfully copied to the backup folder
		assert(Files.list(backupDir).count() == 2)
		assert(Files.exists(backupInstance))
		assert(Files.exists(backupDir.resolve("hotswap.properties")))
		assert(Files.readAllLines(backupInstance).asScala == fileContents)

		// test link was created successfully
		// modify target file, test backup version changes
		val original = Files.readAllBytes(backupInstance)
		Using.Manager { use =>
			val channel = use(FileChannel.open(targetFile, StandardOpenOption.APPEND))
			use(channel.lock)
			val out = use(Channels.newOutputStream(channel))
			out.write("Hello".getBytes)
		}

		assert(!(Files.readAllBytes(backupInstance) sameElements original))
		assert(Files.readAllBytes(targetFile) sameElements Files.readAllBytes(backupInstance))
	}

	test("Setup a hard linked backup directory should fail safe") {
		val (fs, targetFile, filesToBackup) = MockUtils.generateMockFilesystemWinDir()
		val backupDir = fs.getPath("""C:\data\backup""")

		assert(SimpleHotswap.setupInstance(backupDir, targetFile, LinkType.Hard).isEmpty)
	}

	test("Load instance populates a new instance with the correct values") {
		val (fs, targetFile) = MockUtils.generateMockFilesystemWin()
		val instanceFolder = fs.getPath("""C:\data\backup""")

		val generatedInstance = SimpleHotswap.setupInstance(instanceFolder, targetFile, LinkType.Hard)

		assert(generatedInstance.isDefined)
		val initialInstance: SimpleHotswap = generatedInstance.getOrElse(fail)


		val loadedInstance = SimpleHotswap.loadInstance(instanceFolder)

		loadedInstance match {
			case None => fail
			case Some(instance) =>
				assert(instance.instanceFolder.toAbsolutePath.toString == initialInstance
					.instanceFolder
					.toAbsolutePath
					.toString)
				assert(instance.targetFile.toAbsolutePath.toString == initialInstance.targetFile.toAbsolutePath.toString)
				assert(instance.linkType == initialInstance.linkType)
		}
	}

	test("Load instance fails safe when loading a folder not containing the correct file") {
		val (fs, targetFile) = MockUtils.generateMockFilesystemWin()
		val instanceFolder = fs.getPath("""C:\data\backup""")
		assert(SimpleHotswap.loadInstance(instanceFolder).isEmpty)
	}
}
