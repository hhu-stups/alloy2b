#!/bin/sh
./gradlew jar

version='1.2'

for a in $(ls src/test/resources/*.als)
do
  i=`echo "$a" | awk '{print tolower($0)}'`
	java -jar "build/libs/alloy2b-${version}.jar" "$a" -toProlog "${i%.als}.pl";
done
