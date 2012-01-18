name := "logic"

organization := "me.dwins"

version := "0.1"

initialCommands := """
import logic._
import Sentential.symbolicSentences._
import Knowledge._
import Symbolic.{ Atom, symbolAsAtom }
"""

libraryDependencies +=
  "org.scala-tools.testing" %% "scalacheck" % "1.9" % "test"
