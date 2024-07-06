package hmloc

import fastparse._
import fastparse.Parsed.Failure
import fastparse.Parsed.Success

import scala.collection.mutable
import scala.collection.mutable.{Map => MutMap}
import hmloc.utils._
import shorthands._
import org.scalatest.{ParallelTestExecution, funsuite}
import org.scalatest.time._
import org.scalatest.concurrent.{Signaler, TimeLimitedTests}
import os.Path


abstract class ModeType {
  def expectTypeErrors: Bool
  def expectWarnings: Bool
  def expectParseErrors: Bool
  def fixme: Bool
  def showParse: Bool
  def verbose: Bool
  def noSimplification: Bool
  def explainErrors: Bool
  def dbg: Bool
  def dbgParsing: Bool
  def dbgSimplif: Bool
  def fullExceptionStack: Bool
  def stats: Bool
  def stdout: Bool
  def noExecution: Bool
  def debugVariance: Bool
}


case class Mode(
                 expectTypeErrors: Bool = false,
                 expectWarnings: Bool = false,
                 expectParseErrors: Bool = false,
                 fixme: Bool = false,
                 showParse: Bool = false,
                 verbose: Bool = false,
                 noSimplification: Bool = false,
                 explainErrors: Bool = false,
                 dbg: Bool = false,
                 dbgParsing: Bool = false,
                 dbgSimplif: Bool = false,
                 fullExceptionStack: Bool = false,
                 stats: Bool = false,
                 stdout: Bool = false,
                 noExecution: Bool = false,
                 debugVariance: Bool = false,
                 unify: Bool = true,  // unify is on by default
                 unifyDbg: Bool = false,
                 tex: Bool = false,
               ) extends ModeType {
  def isDebugging: Bool = dbg || dbgSimplif
}

object ModeDefaults {
  var allowTypeErrors = false
  var allowParseErrors = false // TODO use
  var showRelativeLineNums = false
  // Parse and check the file with ocaml syntax and semantic rules
  var ocamlMode = false
  // load type definitions of ocaml standard library constructs
  var ocamlLoadLibrary = false
  var noProvs = false
  var allowRuntimeErrors = false

  // Define a map from command strings to functions that modify the mode
  val modeActionMap: Map[String, Mode => Mode] = Map(
    "" -> (_.copy(expectTypeErrors = true)),
    "w" -> (_.copy(expectWarnings = true)),
    "pe" -> (_.copy(expectParseErrors = true)),
    "p" -> (_.copy(showParse = true)),
    "d" -> (_.copy(dbg = true)),
    "dp" -> (_.copy(dbgParsing = true)),
    "ds" -> (_.copy(dbgSimplif = true)),
    "s" -> (_.copy(fullExceptionStack = true)),
    "v" -> (_.copy(verbose = true)),
    "verbose" -> (_.copy(verbose = true)),
    "ex" -> (_.copy(expectTypeErrors = true, explainErrors = true)),
    "explain" -> (_.copy(expectTypeErrors = true, explainErrors = true)),
    "ns" -> (_.copy(noSimplification = true)),
    "no-simpl" -> (_.copy(noSimplification = true)),
    "stats" -> (_.copy(stats = true)),
    "showres" -> (_.copy(stdout = false)),
    "ne" -> (_.copy(noExecution = true)),
    "dv" -> (_.copy(debugVariance = true)),
    "unify" -> (_.copy(unify = true)),
    "unifyDbg" -> (_.copy(unifyDbg = true, unify = true)),
    "tex" -> (_.copy(tex = true))
    // Note: Commands affecting external variables are handled separately
  )

}