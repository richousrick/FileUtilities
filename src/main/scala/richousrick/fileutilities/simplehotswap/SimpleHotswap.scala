package richousrick.fileutilities.simplehotswap

import java.io.IOException
import java.nio.file.{Files, Path}
import java.util.Properties

/**
 * Simple tool to handle hot swapping of different versions of a file or directory
 */
object SimpleHotswap {

	/**
	 * Enum representing the way the versions are loaded for usage
	 * Copy:			local instance is copied from the versions directory
	 * Local instance can be modified without affecting its related version
	 * Hard:			local instance refers to the loaded version using a hard link.
	 * Only supports single file
	 * Any changes to the local instance will be reflected in the stored instance.
	 * Symbolic:	local instance refers to the loaded version using a symbolic link
	 * Similar to hard link however:
	 * Directories are supported
	 * Some tools do not support symbolic links
	 */
	object LinkType extends Enumeration {
		type LinkType = Value
		val Copy, Hard, Symbolic = Value

		/**
		 * Resolves a string representing a LinkType to the relevant enum value
		 *
		 * @param s name of the LinkType to resolve
		 * @return the relevant enum value if one exists otherwise None.
		 */
		def fromString(s: String): Option[Value] = values.find(_.toString == s)
	}

	import LinkType._

	/**
	 * Creates a new config file for the tool to use
	 *
	 * @param backupFile file that the tool should manage backups of
	 * @param linkType   how the current loaded version should be linked to stored versions
	 * @return the properties file
	 */
	def setupConfig(backupFile: String, linkType: LinkType): Properties = {
		val prop = new Properties()
		prop.setProperty("backupFile", backupFile)
		prop.setProperty("useLinks", linkType.toString)
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
