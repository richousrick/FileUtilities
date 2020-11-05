package richousrick.fileutilities.lib

import java.nio.channels.{Channels, FileChannel}
import java.nio.file.{Files, Path, StandardOpenOption}
import java.util.Properties

import scala.util.{Failure, Success, Using}

/**
 * Library enabling simple properties IO
 */
object PropertiesManager {

	/**
	 * Writes the properties to the specified config file
	 *
	 * @param configFile file to write the configuration to
	 * @param prop       properties to write
	 * @return true if the properties were successfully written
	 */
	def writeConfig(configFile: Path, prop: Properties): Boolean =
		Using.Manager { use =>

			// create parent dir if missing
			Files.createDirectories(configFile.getParent)

			val channel = use(FileChannel.open(configFile, StandardOpenOption.WRITE, StandardOpenOption.CREATE))
			use(channel.lock())
			val out = use(Channels.newOutputStream(channel))

			prop.store(out, null)
		} match {
			case Failure(exception) =>
				System.err.println(s"Could not create config file: $exception")
				false

			case Success(_) => true
		}

	/**
	 * Reads properties from a config file
	 *
	 * @param configFile to read the properties file from
	 * @return Some(properties) if successful otherwise None
	 */
	def readConfig(configFile: Path, prop: Properties = new Properties()): Option[Properties] =
		Using.Manager { use =>
			val channel = use(FileChannel.open(configFile, StandardOpenOption.READ))
			use(channel.lock(0L, Long.MaxValue, true))
			val in = use(Channels.newInputStream(channel))
			prop.load(in)
			prop
		} match {
			case Failure(exception) =>
				System.err.println(s"Could not read config file: $exception")
				None

			case Success(properties) => Some(properties)
		}
}








