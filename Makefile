build_install:
	gradle build
	cp build/libs/alloy2b-*.jar ../../prob_prolog/lib
install:
	cp build/libs/alloy2b-*.jar ../../prob_prolog/lib
prolog:
	java -jar build/libs/alloy2b-1.0-SNAPSHOT.jar src/test/resources/FileSystem.als -toProlog out.pl
