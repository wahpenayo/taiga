@echo off
:: wahpenayo at gmail dot com
:: 2018-08-17

::set GC=-XX:-UseParallelGC -XX:+UseSerialGC
::set GC=-XX:+AggressiveHeap -XX:NewRatio=2
set GC=

::set SAFE=-XX:+PrintSafepointStatistics -XX:+PrintGCApplicationStoppedTime -XX:PrintSafepointStatisticsCount=1
set SAFE=

::set PROF=-Xrunhprof:cpu=samples,depth=64,thread=y,doe=y
set PROF=


::set THRUPUT=-d64 -server -XX:+AggressiveOpts -XX:+UseLargePages -Xmn12g
::set THRUPUT=-d64 -server -XX:+AggressiveOpts -Xmn6g
set THRUPUT=-server

:: Leave a couple gb for Windows, Xmx about 2 times Xmn
::set SIZE=30g
set SIZE=12g
::set SIZE=14g
::set SIZE=8g

set XMX=-Xms%SIZE% -Xmx%SIZE%

::set CP=-cp ./src/scripts/clojure;./src/main/clojure;./src/test/clojure;lib/*
set CP=-cp ./src/scripts/clojure;./src/test/java;./src/test/clojure;lib/*

set JAVA_HOME=%JAVA10%
set JAVA="%JAVA_HOME%\bin\java"

set CMD=%JAVA% %THRUPUT% -ea %GC% %SAFE% %PROF% %XMX% %CP% clojure.main %*
::echo %CMD%
%CMD%
