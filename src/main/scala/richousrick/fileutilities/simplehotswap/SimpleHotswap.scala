package richousrick.fileutilities.simplehotswap

import java.io.IOException
import java.nio.file.{Files, Path, Paths}

import richousrick.fileutilities.propmanager.{PropertiesInstance, TypeHandler}

import scala.util.Try

/**
 * Simple tool to handle hot swapping of different versions of a file or directory
 */
object SimpleHotswap {

	// Add support for paths to TypeHandler
	TypeHandler.globalHandlers += TypeHandler.buildHandler('P', (s: String) => Try(Paths.get(s)).toOption)

	/**
	 * Enum representing the way the versions are loaded for usage<br>
	 * &#9;Copy:			local instance is copied from the versions directory<br>
	 * &#9;&#9;Local instance can be modified without affecting its related version<br>
	 * &#9;Hard:			local instance refers to the loaded version using a hard link.<br>
	 * &#9;&#9;Only supports single file<br>
	 * &#9;&#9;Any changes to the local instance will be reflected in the stored instance.<br>
	 * &#9;Symbolic:	local instance refers to the loaded version using a symbolic link. Similar to hard link however:<br>
	 * &#9;&#9;Directories are supported<br>
	 * &#9;&#9;Some tools do not support symbolic links<br>
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
	 * @param targetFile file that the tool should manage instances of
	 * @param linkType   how the current loaded version should be linked to stored instances
	 * @return the properties file
	 */
	def setupConfig(targetFile: Path, linkType: LinkType): PropertiesInstance = {
		val prop = PropertiesInstance()
		prop.setProperty("targetFile", targetFile)
		prop.setProperty("useLinks", linkType)
		prop
	}

	/**
	 * Moves the target file to the instance folder and creates a link in its place referring to its new position
	 *
	 * @param instanceFolder location to store the current instance
	 * @param targetFile     file to be moved and replaced with a link
	 * @param linkType       method of linking the target instance to the current version
	 * @return true if successful
	 */
	def setupInstance(instanceFolder: Path, targetFile: Path, linkType: LinkType): Boolean = {
		if (linkType == LinkType.Hard && Files.isDirectory(targetFile)) {
			System.err.println(s"Hard links are unsupported for directories")
			return false
		}

		val instance = instanceFolder.resolve("currentVersionInstance.current")
		try {
			if (linkType == LinkType.Copy) {
				Files.copy(targetFile, instance)
			} else {
				Files.move(targetFile, instance)
			}
		} catch {
			case e@(_: IOException | _: SecurityException) =>
				System.err.println(s"Could not move current instance to backup folder ${e.getLocalizedMessage}")
				return false
		}

		try {
			linkType match {
				case Hard => Files.createLink(targetFile, instance)
				case Symbolic => Files.createSymbolicLink(targetFile, instance)
				case Copy =>
			}
			true
		} catch {
			case e@(_: IOException | _: SecurityException) =>
				System.err
					.println(s"Could not create a link to the current instance from the backup folder ${e.getLocalizedMessage}")
				false
		}
	}
}
