package snapshot.scheduler

import java.util.Date
import java.util.concurrent._

import scala.util.control.NonFatal

/**
 * Scheduled thread-pool executor implementation that leverages a Quartz CronExpression to calculate future execution times for scheduled tasks.
 */

class CronThreadPoolExecutor(corePoolSize: Int) extends ScheduledThreadPoolExecutor(corePoolSize) with CronExecutorService {
  setThreadFactory(new PoolThreadFactory)

  def schedule(expression: CronExpression)(task: () ⇒ Unit): Unit = {
    require(task != null, "The task cannot be null.")
    new CronTask(this, expression, task).reschedule()
  }
}

class CronTask(pool: ScheduledThreadPoolExecutor, expression: CronExpression, task: () ⇒ Unit) extends Runnable {

  def reschedule(): Unit = {
    val now = new Date
    val time = expression.getNextValidTimeAfter(now)
    if (time != null) {
      pool.schedule(this, time.getTime - now.getTime, TimeUnit.MILLISECONDS)
    }
  }
  def run(): Unit = {
    try {
      task()
    } catch {
      case e @ (_: RejectedExecutionException | _: CancellationException | _: InterruptedException) ⇒ return
      case _: Throwable ⇒
    }
    reschedule()
  }
}

object CronThreadPoolExecutor {
  def apply(corePoolSize: Int): CronThreadPoolExecutor = {
    val executor = new CronThreadPoolExecutor(corePoolSize)
    sys.addShutdownHook(executor.shutdown())
    executor
  }

  implicit def functionZeroAsRunnable(thunk: () ⇒ Unit): Runnable = new Runnable() {
    def run() = {
      try thunk()
      catch {
        case NonFatal(cause) ⇒ //do nothing
      }
    }
  }
}

private class PoolThreadFactory extends ThreadFactory {
  def newThread(r: Runnable): Thread = new Thread(r, "CronPoolExecutorThread")
}
