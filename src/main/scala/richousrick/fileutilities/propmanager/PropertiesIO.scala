package richousrick.fileutilities.propmanager

import java.io.IOException
import java.nio.channels.{Channels, FileChannel}
import java.nio.file.{Files, Path, StandardOpenOption}
import java.util.Properties

import scala.util.{Failure, Success, Using}

/**
 * Library enabling simple properties IO
 */
object PropertiesIO {

	/**
	 * Writes the properties to the specified config file
	 *
	 * @param configFile file to write the configuration to
	 * @param prop       properties to write
	 * @return true if the properties were successfully written
	 */
	def writeConfigSwallow(configFile: Path, prop: Properties): Boolean =
		try {
			writeConfig(configFile, prop)
			true
		} catch {
			case e: IOException => System.err.println(s"Could not create config file: $e"); false
		}

	/**
	 * Writes the properties to the specified config file
	 *
	 * @param configFile file to write the configuration to
	 * @param prop       properties to write
	 */
	def writeConfig(configFile: Path, prop: Properties): Unit =
		Using.Manager { use =>
			// create parent dir if missing
			Option(configFile.getParent) match {
				case Some(parent) => Files.createDirectories(parent)
				case None =>
			}

			val channel = use(FileChannel.open(configFile, StandardOpenOption.WRITE, StandardOpenOption.CREATE))
			use(channel.lock())
			val out = use(Channels.newOutputStream(channel))

			prop.store(out, null)
		} match {
			case Failure(exception) => throw exception
			case Success(_) =>
		}

	/**
	 * Reads properties from a config file
	 *
	 * @param configFile to read the properties file from
	 * @return Some(properties) if successful otherwise None
	 */
	def readConfigSwallow(configFile: Path, prop: Properties = new Properties()): Option[Properties] =
		try {
			Some(readConfig(configFile, prop))
		} catch {
			case e: IOException => System.err.println(s"Could not read config file: $e"); None
		}

	/**
	 * Reads properties from a config file
	 *
	 * @param configFile to read the properties file from
	 * @return the loaded properties
	 */
	def readConfig(configFile: Path, prop: Properties = new Properties()): Properties =
		Using.Manager { use =>
			val channel = use(FileChannel.open(configFile, StandardOpenOption.READ))
			use(channel.lock(0L, Long.MaxValue, true))
			val in = use(Channels.newInputStream(channel))
			prop.load(in)
			prop
		} match {
			case Failure(exception) => throw exception
			case Success(properties) => properties
		}
}








