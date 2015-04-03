name := "AMSSimulator"

version := "0.2.5"

scalaVersion := "2.11.5"

scalacOptions ++= Seq("-unchecked",
					"-deprecation", 
					"-language:reflectiveCalls,postfixOps,implicitConversions", 
					"-feature",
					"-target:jvm-1.6",
					"-encoding", "utf8")

libraryDependencies ++={
	val liftVersion = "2.6.2" 
	val scalaLoggingVersion = "3.1.0"
	val logbackVersion = "1.1.3"
	val scalaConfigVersion = "1.2.1"
	val jodaVersion = "2.7"
	val junitVersion = "4.12"
	val scalaTestVersion = "2.2.4"
	val test = "test"
	 Seq(
	 	"com.typesafe"			%	"config"			% scalaConfigVersion withSources(),
		"net.liftweb" 			%%  	"lift-json"			% liftVersion withSources(),
		"net.liftweb" 			%%  	"lift-json-ext"			% liftVersion withSources(),		
		"com.typesafe.scala-logging"	%% 	"scala-logging" 		% scalaLoggingVersion withSources(),
		"ch.qos.logback"		%  	"logback-classic"		% logbackVersion withSources(),
		"joda-time" 			% 	"joda-time" 			% jodaVersion withSources(),
		"org.joda" 			% 	"joda-convert" 			% "1.7" withSources(),
		"org.joda" 			% 	"joda-money" 			% "0.10.0" withSources(),
		"org.scalatest"			%%	"scalatest"			% scalaTestVersion % test withSources(),
		"junit" 			% 	"junit" 			% junitVersion % test withSources()		
	)
}

resolvers ++=Seq(
	"Spray repository" at "http://repo.spray.io",
	// The Typesafe repository 
	"Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"
)
