package richousrick.fileutilities.lib

import java.nio.file.{FileSystem, Files, Path}

import com.google.common.jimfs.{Configuration, Jimfs}

import scala.util.Random

object MockUtils {

	/**
	 * Generates a mock windows filesystem
	 *
	 * @return a mock windows filesystem containing a simple file
	 */
	def generateMockFilesystemWin(): (FileSystem, Path) = {
		val fs: FileSystem = Jimfs.newFileSystem(Configuration.windows())
		val fileToBackup = fs.getPath("""C:\Users\Someuser\Documents\myFile.txt""")

		Files.createDirectories(fileToBackup.getParent)
		Files.write(fileToBackup,
			"""This is a simple file.
				|It contains more than one line.""".stripMargin.getBytes())

		(fs, fileToBackup)
	}

	/**
	 * Generates a mock windows filesystem
	 *
	 * @return a mock windows filesystem containing a folder with various file contents
	 */
	def generateMockFilesystemWinDir(): (FileSystem, Path, Map[Path, Array[Byte]]) = {
		val fs: FileSystem = Jimfs.newFileSystem(Configuration.windows())
		val fileToBackup = fs.getPath("""C:\Users\Someuser\Documents\myDir""")

		Random.setSeed(1)
		val backupFiles = Map[Path, Array[Byte]](
			fileToBackup.resolve("myFile.txt") -> "This is a simple file.\nIt contains more than one line.".getBytes(),
			fileToBackup.resolve("misc\\SomeFile.txt") -> "This is another file with some stuff in it".getBytes,
			fileToBackup.resolve("misc\\randomBinary") -> Random.nextBytes(1024),
			fileToBackup.resolve("misc\\someScript.bash") -> "echo \"The Third File.\nThe purpose of this is to emulate multiple nested files.\""
				.getBytes()
		)

		Files.createDirectories(fileToBackup.resolve("misc"))
		backupFiles.foreach(f => Files.write(f._1, f._2))

		(fs, fileToBackup, backupFiles)
	}
}
