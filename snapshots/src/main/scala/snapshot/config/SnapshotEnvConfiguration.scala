package snapshot.config

import java.util.concurrent.TimeUnit

import base.conf.EnvConfig

class SnapshotEnvConfiguration extends EnvConfig {
  val attemptsThreshold = envConfiguration.config.getInt("snapshots.attempts.threshold")
  val timeBetweenAttempts = envConfiguration.config.getDuration("snapshots.time.between.attempts", TimeUnit.MILLISECONDS)
  val cronThreadPoolExecutorPoolSize = envConfiguration.config.getInt("snapshots.cron.thread.pool.size")
  val snapshotsPath = envConfiguration.config.getString("snapshots.files-path")
  val timeBetweenReloadForOutdatedFile = envConfiguration.config.getDuration("snapshots.time.between.reload.for-outdated-file", TimeUnit.MILLISECONDS)
  val maxDifferencePercentage = envConfiguration.config.getInt("snapshots.max.difference.percentage")
}
