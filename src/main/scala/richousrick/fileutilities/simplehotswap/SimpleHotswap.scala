package richousrick.fileutilities.simplehotswap

import java.io.IOException
import java.nio.file.{Files, Path}
import java.util.Properties

/**
 * Simple tool to handle hot swapping of different versions of a file or directory
 */
object SimpleHotswap {

	/**
	 * Creates a new config file for the tool to use
	 *
	 * @param backupFile file that the tool should manage backups of
	 * @param useLinks   if the backup file should be stored with the backups and replaced with a hard link
	 * @return the properties file
	 */
	def setupConfig(backupFile: String, useLinks: Boolean): Properties = {
		val prop = new Properties()
		prop.setProperty("backupFile", backupFile)
		prop.setProperty("useLinks", useLinks + "")
		prop
	}

	/**
	 * Moves the backup file to the backup folder and creates a link in its place referring to its new position
	 *
	 * @param backupFolder location to store the current instance
	 * @param targetFile   file to be moved and replaced with a link
	 * @param hard         if true the link will be hard, otherwise it will be symbolic
	 * @return true if successful
	 */
	def initLink(backupFolder: Path, targetFile: Path, hard: Boolean): Boolean = {
		if (hard && Files.isDirectory(targetFile)) {
			System.err.println(s"Hard links are unsupported for directories")
			return false
		}

		val backupInstance = backupFolder.resolve("currentVersionInstance.current")
		try {
			Files.move(targetFile, backupInstance)
		} catch {
			case e@(_: IOException | _: SecurityException) =>
				System.err.println(s"Could not move current instance to backup folder ${e.getLocalizedMessage}")
				return false
		}

		try {
			if (hard)
				Files.createLink(targetFile, backupInstance)
			else
				Files.createSymbolicLink(targetFile, backupInstance)
			true
		} catch {
			case e@(_: IOException | _: SecurityException) =>
				System.err
					.println(s"Could not create a link to the current instance from the backup folder ${e.getLocalizedMessage}")
				false
		}
	}
}
