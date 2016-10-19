package snapshot

import java.util.concurrent.ScheduledExecutorService
import java.util.concurrent.TimeUnit.{ MILLISECONDS ⇒ millis }
import java.util.concurrent.atomic.AtomicReference

import base.LazyLoggerSupport
import snapshot.config.SnapshotEnvConfiguration
import snapshot.scheduler.{ CronExecutorService, CronExpression }

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.collection.{ TraversableOnce, immutable, mutable }
import scala.concurrent.duration.Duration
import scala.concurrent.{ Await, ExecutionContext, Future }
import scala.reflect.ClassTag
import scala.util.control.NonFatal
import scala.util.{ Failure, Success, Try }

/**
 * @tparam A Type of entity returned from source(fetch)
 * @tparam S Type of entity to save as snapshot
 */
abstract class BaseSnapshot[A, S: ClassTag] extends SnapshotEnvConfiguration with LazyLoggerSupport {

  private val snapshot = new AtomicReference[S]()

  protected def set(data: S) = snapshot.set(data)

  def getAll: S = snapshot.get()

  private def schedule(ex: CronExecutorService, version: String): Unit = {
    require(cronExpression != null, "The CronExpression cannot be null.")
    ex.schedule(new CronExpression(cronExpression)) { () ⇒ reload(version) }
  }

  private def sanitizeName(name: String): String = name.split("\\$").last

  final def reload(version: String, attempts: Int = attemptsThreshold): Unit = {
    @tailrec
    def go(attemptsLeft: Int): Unit = build match {
      case Success(data) ⇒
        logger.info(s"Successful reloading  ${sanitizeName(getClass.getSimpleName)} snapshot.")
        set(data)
        SnapshotSerializer.saveSnapshotFile(snapshotFileName(version), data)
      case Failure(NonFatal(cause)) ⇒
        if (attemptsLeft > 0) {
          logger.error(s"Error trying to reload ${sanitizeName(getClass.getSimpleName)} snapshot. Retrying in ${millis.toSeconds(timeBetweenAttempts)} seconds, $attemptsLeft left.", cause)
          Thread.sleep(timeBetweenAttempts)
          go(attemptsLeft - 1)
        } else {
          val message = s"Error trying to reload ${sanitizeName(getClass.getSimpleName)} snapshot. $attemptsLeft retry attempts are made."
          logger.error(message)
          throw new RuntimeException(message)
        }
    }
    go(attempts)
  }

  def initialize(version: String)(implicit ex: CronExecutorService with ScheduledExecutorService): Unit =
    SnapshotSerializer.getSnapshotFile(this, version) match {
      case Success(data) ⇒
        logger.info(s"Successful initialization of ${sanitizeName(getClass.getSimpleName)} snapshot.")
        set(data)
        schedule(ex, version)
      case Failure(NonFatal(cause)) ⇒ throw new RuntimeException(s"Error trying to initialize ${sanitizeName(getClass.getSimpleName)}.", cause)
    }

  def snapshotFileName(version: String): String = this.getClass.getName + version
  def cronExpression: String

  /**
   * Fetches data from source to build the snapshot
   */
  def fetch: A

  /**
   * Converts returned data [[A]] from source to type [[S]] to be saved in snapshot
   * @return
   */
  def converter: (A ⇒ S)

  def build: Try[S] = {
    Success(converter(fetch))
  }
}

/**
 * @tparam S Type of entity to save as snapshot
 */
abstract class SimpleSnapshot[S: ClassTag] extends BaseSnapshot[S, S] {
  def converter = Predef.identity[S]
}

/**
 * Utility base snapshot when fetch returns a TraversableOnce
 * @tparam A Type of entity returned from source(fetch)
 * @tparam S Type of entity to save as snapshot
 */
abstract class TraversableSourceSnapshot[A, S: ClassTag] extends BaseSnapshot[TraversableOnce[A], S] {}

/**
 * Snapshot that builds several maps from the source using the supplied keyFunctions
 * Maps are [[scala.collection.concurrent.TrieMap]]s by default, override `emptyMap` if necessary
 * For example, having a list of people one could build two maps: one with a database id and
 * another one with it's passport id.
 * It's perfect for cases when you use the same source and need data structured in different ways
 * @tparam K Type of the key to be used in the map
 * @tparam V Type of the value to be used in the map
 */
abstract class GroupingSnapshot[K, V] extends TraversableSourceSnapshot[V, Seq[collection.Map[K, V]]] with DefaultMapping[K, V] {
  def keyFunctions: Seq[(V ⇒ K)]
  def emptyMap: collection.Map[K, V] = TrieMap.empty

  override def set(maps: Seq[collection.Map[K, V]]) = super.set(Option(defaultMapping) match {
    case Some(f) ⇒ maps.map {
      case m @ (c: mutable.Map[K, V])   ⇒ m.asInstanceOf[mutable.Map[K, V]] withDefault f
      case m @ (c: immutable.Map[K, V]) ⇒ m.asInstanceOf[immutable.Map[K, V]] withDefault f
      case m                            ⇒ m
    }
    case _ ⇒ maps
  })

  override def converter: TraversableOnce[V] ⇒ Seq[collection.Map[K, V]] = _.foldLeft(keyFunctions.map(_ ⇒ emptyMap)) { (accum, item) ⇒
    accum.zip(keyFunctions).map { t: (collection.Map[K, V], (V) ⇒ K) ⇒
      append(t._1, t._2(item) -> item)
    }
  }

  def append(map: collection.Map[K, V], kv: (K, V)): collection.Map[K, V] = {
    map match {
      case c: mutable.Map[K, V] ⇒ map.asInstanceOf[mutable.Map[K, V]] += kv
      case _                    ⇒ map + kv
    }
  }

  def apply(indexOrKeyFunction: Either[Int, (V ⇒ K)]) = getAll(indexOrKeyFunction.right.map(keyFunctions.indexOf).merge)
  def apply(indexOrKeyFunction: Either[Int, (V ⇒ K)], key: K): V = apply(indexOrKeyFunction)(key)
  def apply(indexOrKeyFunction: Either[Int, (V ⇒ K)], keys: Seq[K]): Iterable[V] = apply(indexOrKeyFunction).filterKeys(keys.contains).values

}

trait DefaultMapping[K, V] {
  def defaultMapping: (K ⇒ V) = null
}

trait NoticeErrorKeyNotFoundDefaultMapping[K, V] extends DefaultMapping[K, V] with LazyLoggerSupport {
  def defaultValue: (K ⇒ V)
  override val defaultMapping: (K ⇒ V) = key ⇒ {
    logger.error(s"Missing key '$key' in catalog ${getClass.getSimpleName.replaceAll("(?i)snapshot", "")}")
    defaultValue(key)
  }
}

/**
 * Snapshot that builds a map from the source using the supplied keyFunction.
 * It is basically a [[snapshot.GroupingSnapshot]] with only one keyFunction
 * @tparam K Type of the key to be used in the map
 * @tparam V Type of the value to be used in the map
 */
abstract class GroupedSnapshot[K, V] extends GroupingSnapshot[K, V] {
  def keyFunction: (V ⇒ K)
  override def keyFunctions = Seq(keyFunction)

  def get() = getAll.head
  def apply(k: K) = super.apply(Left(0), k)
  def keys: Iterable[K] = get().keys
  def values: Iterable[V] = get().values
}

/**
 * Trait that provides support for snapshots that need several calls to a service.
 *
 * It groups all responses in a map, using '''P=>K''' function as the keys, and values are the result of the service call.
 *
 * Maps are [[scala.collection.concurrent.TrieMap]]s by default, override `emptyMap` if necessary.
 * @tparam P Type of payload used to call the service
 * @tparam K Type of the key to be used in the map
 * @tparam V Type of the value to be used in the map
 */
trait MultipleFetch[P, K, V] {
  def payloads: Traversable[P]
  def keyTranslator: (P ⇒ K)
  def fetchOne(payload: P): V
  def timeout: Duration
  def emptyMap: collection.Map[K, V] = TrieMap.empty

  def fetch(implicit ec: ExecutionContext): collection.Map[K, V] = {
    payloads.map(p ⇒ (keyTranslator(p), Future(fetchOne(p)))).foldLeft(emptyMap) { (map, keyFuture) ⇒
      append(map, keyFuture._1 -> Await.result(keyFuture._2, timeout))
    }
  }

  def append(map: collection.Map[K, V], kv: (K, V)): collection.Map[K, V] = {
    map match {
      case c: mutable.Map[K, V] ⇒ map.asInstanceOf[mutable.Map[K, V]] += kv
      case _                    ⇒ map + kv
    }
  }
}

/**
 * SamePKMultipleFetch is a [[snapshot.MultipleFetch]] that uses the payload class as the map key class
 * @tparam K Type of payload and key to be used in the map
 * @tparam V Type of the value to be used in the map
 */
trait SamePKMultipleFetch[K, V] extends MultipleFetch[K, K, V] {
  override def keyTranslator: (K) ⇒ K = Predef.identity[K]
}

///**
//  * Support for language snapshots that need several calls to a service
//  * It is basically a [[snapshot.GroupingSnapshot]] with only one keyFunction
//  * @tparam V Type of the value to be used in the map
//  */
//abstract class MultiLanguageSnapshot[V] extends SimpleSnapshot[collection.Map[Lang, V]] with SamePKMultipleFetch[Lang, V] {
//  def languageCodes: Traversable[String]
//  override lazy val payloads = languageCodes.map(Lang.apply)
//  def apply(k: Lang): V = getAll(k)
//}

/**
 * Deprecated, left for retrocompatibility
 * @tparam S Type of entity to save as snapshot
 */
@Deprecated
abstract class Snapshot[S: ClassTag] extends BaseSnapshot[Object, S] {
  def fetch = null
  def converter = null
}
