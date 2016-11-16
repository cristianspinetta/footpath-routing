Footpath Routing
=================================
Footpath Routing is an app to generate routes for pedestrians based on sidewalks.

## Getting Started

### The dependencies:

* Java 1.8
* Scala 2.11.8
* SBT 0.13.11

### Setting up the project and Run:

* Run `sbt navigation-api/run` from the project root.

### Generate a Fat Jar with Assembly:

* Run `sbt navigation-api/assembly` from the project root.

The fully executable JAR will be in `/navigation-api/target/scala-2.11/` ready to rock.

### Running the App

The following is an example to run the app in production environment:

```
java \
    -Duser.timezone=GMT-0 \
    -DenvironmentOverride=/path/to/your/environment-override.conf \
    -Denvironment=prod \
    -Dcom.sun.management.jmxremote.ssl=false \
    -Dcom.sun.management.jmxremote.authenticate=false \
    -Dcom.sun.management.jmxremote.port=29290 \
    -Xms1024m \
    -Xmx2048m \
    -verbose:gc \
    -XX:+PrintGCDetails \
    -XX:+PrintGCTimeStamps \
    -XX:+PrintGCDateStamps \
    -Xloggc:./gc.log \
    -XX:+HeapDumpOnOutOfMemoryError \
    -XX:HeapDumpPath=./dumps/heap-dump.hprof \
    -XX:-OmitStackTraceInFastThrow \
    -XX:+DisableExplicitGC \
    -XX:+TieredCompilation \
    -XX:+UseConcMarkSweepGC \
    -XX:CMSInitiatingOccupancyFraction=40 \
    -XX:+UseCMSInitiatingOccupancyOnly \
    -XX:+CMSScavengeBeforeRemark \
    -XX:NewRatio=1 \
    -jar /path/to/footpath-routing-api.jar
```

**Environment available:** *dev* and *prod*.

### Running tests

* **Unit tests:** `sbt test`
* **Benchmarks:** `sbt bench:test`
* **DB with local MariaDB:** `sbt bdt:test`

### Snapshots settings

The name of the available snapshots:

* `street-vertex`
* `sidewalk-vertex`
* `stop`
* `ramp`

This is the configuration available for the snapshots:

```
snapshots {
  attempts.threshold = 5
  time.between.attempts = 30 seconds
  time.between.reload.for-outdated-file = 60 seconds
  cron.thread.pool.size = 10
  files-path = "/tmp/snapshots"
  max.difference.percentage = 80

  street-vertex.cron-expression = "0 0 0 1/60 * ? *"
  sidewalk-vertex.cron-expression = "0 0 0 1/60 * ? *"
  stop.cron-expression = "0 0 0 1/60 * ? *"
  ramp.cron-expression = "0 0 0 1/60 * ? *"
  
  loading-all-timeout = 10m // 10 minutes
  parallel-loading = 4
}
```
When the app is started, it searches in the folder specified by `snapshots.files-path` key the snapshot files. 
If exists and it is in valid time, it uses them, otherwise it creates them through fetching the data from the suitable source.
As you can notice, it's possible specify the time and the place where the snapshots live.

### Reloading data from DB

##### Reload **All Cached Data** from the primary storage:

```
curl -i -X POST 'http://localhost:9000/private/map/reload'
```

##### Reload **All Snapshots** from the primary storage:

```
curl -i -X POST 'http://localhost:9000/private/snapshot/reload/all'
```

##### Reload **A Single Snapshot** from the primary storage:

```
curl -i -X POST 'http://localhost:9000/private/snapshot/reload/street-vertex'
```

The last segment in the endpoint is the snapshot name.
