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

**Environment** available: *dev*, *prod*.