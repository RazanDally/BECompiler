package hmloc

import fastparse._
import fastparse.Parsed.Failure
import fastparse.Parsed.Success

import scala.collection.mutable
import scala.collection.mutable.{Map => MutMap}
import scala.collection.mutable.Buffer
import hmloc.utils._
import shorthands._
import org.scalatest.{ParallelTestExecution, funsuite}
import org.scalatest.time._
import org.scalatest.concurrent.{Signaler, TimeLimitedTests}
import os.Path


object TestHelperConsts{
  val outputMarker = "//│ "
  val lexicalErrorText = s"╔══[LEXICAL ERROR] "
  val parseErrorText = s"╔══[PARSE ERROR] "
  val basicErrorText = s"╔══[ERROR] "
  val warningText = s"╔══[WARNING] "

  val prepre = "║  "

}
