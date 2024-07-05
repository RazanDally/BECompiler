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
