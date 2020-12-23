package richousrick.fileutilities.simplehotswap

import java.io.IOException
import java.nio.file.{Files, Path, Paths}

import richousrick.fileutilities.propmanager.{PropertiesInstance, TypeHandler}
import richousrick.fileutilities.simplehotswap.LinkType.LinkType

import scala.util.Try

/**
 * Simple tool to handle hot swapping of different versions of a file or directory
 */
object SimpleHotswap {

	/**
	 * Sets up a fresh SimpleHotswap instance
	 *
	 * @param instanceFolder location to store the current instance
	 * @param targetFile     file to be moved and replaced with a link
	 * @param linkType       method of linking the target instance to the current version
	 * @return The generated instance if successful
	 */
	def setupInstance(instanceFolder: Path, targetFile: Path, linkType: LinkType): Option[SimpleHotswap] = {
		Files.createDirectories(instanceFolder)

		// assert instanceFolder is empty
		if (Files.list(instanceFolder).count != 0) {
			return None
		}

		val instance = new SimpleHotswap(instanceFolder, targetFile, linkType)
		// backup current instance
		if (!instance.createInstance("currentVersionInstance.current", overwrite = false, initial = true)) {
			return None
		}

		if (!instance.loadInstance("currentVersionInstance.current")) {
			return None
		}
		instance.storeConfig()
		Some(instance)
	}

	/**
	 * Loads an instance from a specified folder
	 *
	 * @param instanceFolder Folder storing the hotswap.properties file that should be loaded
	 * @return An instances loaded from the specified file. If one exists. None otherwise
	 */
	def loadInstance(instanceFolder: Path): Option[SimpleHotswap] = {
		val propertiesFile = instanceFolder.resolve("hotswap.properties")

		if (Files.notExists(propertiesFile)) {
			// fail if properties file does not exist
			return None
		}

		val properties = initProperties
		if (!properties.load(propertiesFile)) {
			// fail if cannot load properties from the file
			return None
		}

		// load properties from the file and use to create instance
		(properties.getProperty[Path]("targetFile"), properties.getProperty[LinkType]("useLinks")) match {
			case (Some(targetFile: Path), Some(linkType: LinkType)) => Some(new SimpleHotswap(instanceFolder,
				targetFile,
				linkType))
			case _ => None
		}
	}

	/**
	 * Creates a properties instance with support for Paths
	 *
	 * @return a properties instance with support for Paths
	 */
	private def initProperties: PropertiesInstance =
		PropertiesInstance(Set[TypeHandler[_]](TypeHandler.buildHandler('P',
			(s: String) => Try(Paths.get(s)).toOption)))
}

/**
 * Class used to create various instances of a file / folder and load them
 *
 * @param instanceFolder location to store the current instance
 * @param targetFile     file to be moved and replaced with a link
 * @param linkType       method of linking the target instance to the current version
 */
class SimpleHotswap(val instanceFolder: Path, val targetFile: Path, val linkType: LinkType) {

	/**
	 * Creates a backup of the current state of the file and stores it to the file with the specified name
	 *
	 * @param instanceName name to store the new instance
	 * @param overwrite    if true any instances with that name will be overwritten. Otherwise if an instance with the same name exists
	 *                     , the method will fail and return false.
	 * @return true if successful
	 */
	def createInstance(instanceName: String, overwrite: Boolean = false): Boolean =
		createInstance(instanceName, overwrite, initial = false)

	/**
	 * Creates a backup of the current state of the file and stores it to the file with the specified name
	 *
	 * @param instanceName name to store the new instance
	 * @param overwrite    if true any instances with that name will be overwritten. Otherwise if an instance with the same name exists
	 *                     , the method will fail and return false.
	 * @param initial      if this is the first instance
	 * @return true if successful
	 */
	private def createInstance(instanceName: String, overwrite: Boolean, initial: Boolean): Boolean = {
		if (linkType == LinkType.Hard && Files.isDirectory(targetFile)) {
			System.err.println(s"Hard links are unsupported for directories")
			return false
		}

		val instance = instanceFolder.resolve(instanceName)

		// test if instance already exists
		if (!overwrite && Files.exists(instance)) {
			return false
		}

		try {
			if (initial && linkType != LinkType.Copy) {
				Files.move(targetFile, instance)
			} else {
				Files.copy(targetFile, instance)
			}
			true
		} catch {
			case e@(_: IOException | _: SecurityException) =>
				System.err.println(s"Could not move current instance to backup folder ${e.getLocalizedMessage}")
				false
		}
	}

	/**
	 * Replaces the current instance of the file with the one specified
	 *
	 * @param name name of the instance
	 */
	def loadInstance(name: String): Boolean = {
		val instance: Path = instanceFolder.resolve(name)
		if (!Files.exists(instance)) {
			return false
		}

		try {
			//TODO: Modify to work with copied directories
			Files.deleteIfExists(targetFile)
			linkType match {
				case LinkType.Hard => Files.createLink(targetFile, instance)
				case LinkType.Symbolic => Files.createSymbolicLink(targetFile, instance)
				case LinkType.Copy => Files.copy(instance, targetFile)
			}
			true
		} catch {
			case e@(_: IOException | _: SecurityException) =>
				System.err
					.println(s"Could not create a link to the current instance from the backup folder")
				e.printStackTrace()
				false
		}
	}


	/**
	 * Stores the config
	 */
	def storeConfig(): Boolean = {
		val prop = SimpleHotswap.initProperties
		prop.setProperty("targetFile", targetFile)
		prop.setProperty("useLinks", linkType)
		prop.write(instanceFolder.resolve("hotswap.properties"))
	}
}


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
