name := "AMSSimulator"

version := "0.1.0"

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
		"io.spray" 				%  	"spray-can" 		% sprayVersion withSources(),
		"io.spray" 				%  	"spray-http" 		% sprayVersion withSources(),
		"io.spray" 				%  	"spray-routing" 	% sprayVersion withSources(),
		"com.typesafe.akka"		%%	"akka-actor" 		% akkaVersion withSources(),
		"com.typesafe.akka" 	%% 	"akka-slf4j" 		% akkaVersion withSources(),
		"com.typesafe.slick" 	%% 	"slick"				% slickVersion withSources(),
		"net.liftweb" 			%%  "lift-json"			% liftVersion withSources(),
		"net.liftweb" 			%%  "lift-json-ext"		% liftVersion withSources(),
		"ch.qos.logback"		%  	"logback-classic"	% logbackVersion withSources(),
		"joda-time" 			% 	"joda-time" 		% jodaVersion withSources(),
		"org.joda" 				% 	"joda-convert" 		% "1.5" withSources(),
		"org.joda" 				% 	"joda-money" 		% "0.9" withSources(),
		"junit" 				% 	"junit" 			% junitVersion withSources(),
		"org.scalatest"			%%	"scalatest"			% scalaTestVersion withSources(),		
		"com.typesafe" 			%% 	"scalalogging-slf4j" % "1.0.1"
	)
}

resolvers ++=Seq(
	"Spray repository" at "http://repo.spray.io"
)
