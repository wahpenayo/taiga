#! /bin/zsh
# John Alan McDonald
# 2013-10-11
# assuming clojure-xxx.jar on the CP

#export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.7.0_51.jdk/Contents/Home

export MAIN=src/main/clojure
export SCRIPTS=src/scripts/clojure
export TUTORIAL=src/main/lyx/tu-torial

export GC1="-XX:+UseParNewGC -XX:ParallelCMSThreads=4"
export GC="-XX:+DoEscapeAnalysis -XX:+AggressiveOpts -XX:AutoBoxCacheMax=32639 -XX:NewRatio=3"

export PROF="-Xrunhprof:cpu=samples,depth=16,thread=y,doe=y"
export PROF=

#set size appropriate to your runtime environment here
export SIZE=1g

export XMX="-Xms${SIZE} -Xmx${SIZE}"

export JAVA_HOME=/usr
export MYCLASSPATH=$CLASSPATH

export CP="-cp ${MYCLASSPATH}:./src/scripts/clojure:lib/*"
export ASSERT="-ea -da:org.geotools... -da:org.opengis..."
export JAVA="${JAVA_HOME}/bin/java"
export CLJ="${JAVA} ${ASSERT} ${GC} ${PROF} ${XMX} ${CP} clojure.main"
${CLJ} $@
