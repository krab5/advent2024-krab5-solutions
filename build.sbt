name := "Advent of code 2024"
version := "0.0.1"

scalacOptions ++= Seq("-explain", "-feature")

Compile / doc / target := file("doc")
Compile / doc / scalacOptions ++= Seq("-project", "AoC-2024", "-groups")

scalaVersion := "3.3.4"


