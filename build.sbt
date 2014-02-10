import AssemblyKeys._ // put this at the top of the file

assemblySettings 

name := "AMSSimulator"

version := "0.2.5"

scalaVersion := "2.10.3"

scalacOptions ++= Seq("-deprecation", "-feature","-target:jvm-1.6")

libraryDependencies ++={
	val sprayVersion = "1.2-RC4"
	val akkaVersion = "2.2.3"
	val slickVersion = "2.0.0-M3"
	val liftVersion = "2.5.1" 
	val logbackVersion = "1.0.13"
	val jodaVersion = "2.3"
	val junitVersion = "4.11"
	val scalaTestVersion = "2.0"
	 Seq(
		"net.liftweb" 			%%  "lift-json"			% liftVersion withSources(),
		"net.liftweb" 			%%  "lift-json-ext"		% liftVersion withSources(),
		"ch.qos.logback"		%  	"logback-classic"	% logbackVersion withSources(),
		"joda-time" 			% 	"joda-time" 		% jodaVersion withSources(),
		"org.joda" 				% 	"joda-convert" 		% "1.5" withSources(),
		"org.joda" 				% 	"joda-money" 		% "0.9" withSources(),
		"junit" 				% 	"junit" 			% junitVersion withSources(),
		"org.scalatest"			%%	"scalatest"			% scalaTestVersion withSources(),		
		"com.typesafe" 			%% 	"scalalogging-slf4j" % "1.0.1" withSources(),
		"com.typesafe"			%	"config"			% "1.0.2" withSources()
	)
}

resolvers ++=Seq(
	"Spray repository" at "http://repo.spray.io",
	// The Typesafe repository 
	"Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"
)

jarName in assembly := "amsSimulatorV0.2.5.jar"

mainClass in assembly := Some("ams.sim.feeds.BTRCreator")
