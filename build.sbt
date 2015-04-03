name := "AMSSimulator"

version := "0.2.5"

scalaVersion := "2.10.3"

scalacOptions ++= Seq("-unchecked",
					"-deprecation", 
					"-language:reflectiveCalls,postfixOps,implicitConversions", 
					"-feature",
					"-target:jvm-1.6",
					"-encoding", "utf8")

libraryDependencies ++={
	val liftVersion = "2.5.1" 
	val scalaLoggingVersion = "1.1.0"
	val logbackVersion = "1.1.1"
	val scalaConfigVersion = "1.2.0"
	val jodaVersion = "2.3"
	val junitVersion = "4.11"
	val scalaTestVersion = "2.1.0-RC2"
	val test = "test"
	 Seq(
	 	"com.typesafe"			%	"config"			% scalaConfigVersion withSources(),
		"net.liftweb" 			%%  "lift-json"			% liftVersion withSources(),
		"net.liftweb" 			%%  "lift-json-ext"		% liftVersion withSources(),		
		"com.typesafe" 			%% 	"scalalogging-slf4j" % scalaLoggingVersion withSources(),
		"ch.qos.logback"		%  	"logback-classic"	% logbackVersion withSources(),
		"joda-time" 			% 	"joda-time" 		% jodaVersion withSources(),
		"org.joda" 				% 	"joda-convert" 		% "1.5" withSources(),
		"org.joda" 				% 	"joda-money" 		% "0.9" withSources(),
		"org.scalatest"			%%	"scalatest"			% scalaTestVersion % test withSources(),
		"junit" 				% 	"junit" 			% junitVersion % test withSources()		
	)
}

resolvers ++=Seq(
	"Spray repository" at "http://repo.spray.io",
	// The Typesafe repository 
	"Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"
)
