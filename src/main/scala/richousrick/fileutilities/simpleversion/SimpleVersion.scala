package richousrick.fileutilities.simpleversion

import java.util.Properties

object SimpleVersion {

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
}
