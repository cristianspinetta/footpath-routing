package snapshot

import java.io.File
import java.util.Date
import java.util.concurrent.{ ScheduledExecutorService, TimeUnit }

import base.LazyLoggerSupport
import snapshot.config.SnapshotEnvConfiguration
import snapshot.scheduler.{ CronExpression, CronThreadPoolExecutor }

import scala.reflect.ClassTag
import scala.util.{ Failure, Success, Try }

trait Snapshots extends SnapshotEnvConfiguration {

  implicit val scheduler = CronThreadPoolExecutor(cronThreadPoolExecutorPoolSize)

  /*It must be overridden by the snapshots in the app*/
  def snapshots: Seq[BaseSnapshot[_, _]] = Seq.empty

  /*It must be overridden by the version of the app*/
  def version: String = ""

  def initialize(): Unit = snapshots.foreach(_.initialize(version))

  def reload(): Try[Unit] = Try { snapshots.foreach(_.reload(version)) }

  def shutdown(): Unit = scheduler.shutdown()
}

object SnapshotSerializer extends SnapshotEnvConfiguration with LazyLoggerSupport {
  private val snapshotFilenameExt = ".gz"

  def getSnapshotFile[A, S: ClassTag](snapshot: BaseSnapshot[A, S], version: String)(implicit ex: ScheduledExecutorService): Try[S] = {
    val filename = s"${snapshot.snapshotFileName(version)}$snapshotFilenameExt"
    val file = new File(snapshotsPath, filename)

    if (file.exists()) {
      logger.info(s"reading ${snapshot.getClass.getSimpleName} from file: $snapshotsPath$filename")
      Try {
        val stream = ObjectSerializer.gzipToByteArray(file)
        ObjectSerializer.deserialize[S](stream)
      } match {
        case Success(data) ⇒
          if (!isUpdated(file, snapshot)) {
            logger.info(s"file: $snapshotsPath$filename for ${snapshot.getClass.getSimpleName} is not updated, scheduling a reload")
            ex.schedule(new Runnable {
              override def run(): Unit = snapshot.reload(version)
            }, timeBetweenReloadForOutdatedFile, TimeUnit.MILLISECONDS)
          }
          Success(data)
        case Failure(_) ⇒ loadSnapshotData(snapshot, version)
      }
    } else {
      loadSnapshotData(snapshot, version)
    }
  }

  private def loadSnapshotData[A, S: ClassTag](snapshot: BaseSnapshot[A, S], version: String): Try[S] = {
    logger.info(s"Loading snapshot ${snapshot.getClass.getSimpleName}")
    val data = snapshot.build
    if (data.isSuccess)
      saveSnapshotFile(snapshot.snapshotFileName(version), data.get)
    data
  }
  private def isUpdated[A, S: ClassTag](file: File, snapshot: BaseSnapshot[A, S]): Boolean = {
    new CronExpression(snapshot.cronExpression).getNextValidTimeAfter(new java.util.Date(file.lastModified())).after(new java.util.Date())
  }

  // Saves to file
  def saveSnapshotFile[A, S: ClassTag](fileName: String, data: S) = {
    val fileNameWithExt = s"$fileName$snapshotFilenameExt"
    Try {
      val snapshotsFolder = new File(snapshotsPath)
      if (!snapshotsFolder.exists()) {
        snapshotsFolder.mkdirs()
      }

      val file: File = new File(snapshotsPath, s"$fileNameWithExt")
      val currentSnapshotData = getSnapshotData(file)
      val serializedNewData = ObjectSerializer.serialize(data)

      currentSnapshotData match {
        case Some(currentData) if serializedNewData.length < getMinSizeAcceptable(currentData.length) ⇒
          val message = s"[SNAPSHOT NOT UPDATED]: $fileName reload size (${serializedNewData.length} bytes) is lower to the minimum size accepted ($maxDifferencePercentage% of the current). Actual size is ${currentData.length} bytes. The Snapshot was not updated."
          logger.error(message)
          //update the modification date of file to prevent reload
          file.setLastModified(new Date().getTime)
        case _ ⇒ writeFile(file, serializedNewData)
      }

    } match {
      case Failure(e) ⇒
        logger.error(s"Error creating snapshot file $fileNameWithExt: ", e)
      //don't throw exception to prevent application fail
      //throw e
      case _ ⇒ logger.info(s"$fileNameWithExt saved successfully to path: $snapshotsPath")
    }
  }

  private def getSnapshotData(file: File): Option[Array[Byte]] = {
    if (file.exists())
      Some(ObjectSerializer.gzipToByteArray(file))
    else
      None
  }

  private def getMinSizeAcceptable(originalSize: Long) = originalSize * (maxDifferencePercentage.toFloat / 100)

  private def writeFile(file: File, serializedData: Array[Byte]) = ObjectSerializer.byteArrayToGZipFile(serializedData, file)

}