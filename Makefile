build_install:
	gradle build
	cp build/libs/alloy2b-*.jar ../../prob_prolog/lib
install:
	cp build/libs/alloy2b-*.jar ../../prob_prolog/lib
