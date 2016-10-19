package snapshot.scheduler

import java.util.concurrent.ExecutorService

/**
 * Executor service that schedules a runnable task for execution via a cron expression.
 */
trait CronExecutorService extends ExecutorService {

  /**
   * Schedules the specified task to execute according to the specified cron expression.
   * @param expression a cron expression
   * @param block code to schedule
   */
  def schedule(expression: CronExpression)(block: () ⇒ Unit)
}