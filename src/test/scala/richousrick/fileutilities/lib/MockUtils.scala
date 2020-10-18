package richousrick.fileutilities.lib

import java.nio.file.{FileSystem, Files, Path}

import com.google.common.jimfs.{Configuration, Jimfs}

object MockUtils {

	/**
	 * Generates a mock windows filesystem
	 *
	 * @return a mock windows filesystem containing a simple file and destination folder
	 */
	def generateMockFilesystemWin(): (FileSystem, Path, Path) = {
		val fs: FileSystem = Jimfs.newFileSystem(Configuration.windows())
		val fileToBackup = fs.getPath("""C:\Users\Someuser\Documents\myFile.txt""")
		val backupDir = fs.getPath("""C:\data\backup""")

		Files.createDirectories(fileToBackup.getParent)
		Files.write(fileToBackup,
			"""This is a simple file.
				|It contains more than one line.""".stripMargin.getBytes())

		Files.createDirectories(backupDir)
		(fs, fileToBackup, backupDir)
	}
}
