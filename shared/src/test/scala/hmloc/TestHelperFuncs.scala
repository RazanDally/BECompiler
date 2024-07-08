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

import TestHelperConsts._

object TestHelperFuncs{

  /**
   * Get the test string for a test
   * @param basePath basePath list of strings
   * @param file Path for the file
   * @return the test string
   */
  def getTestStr(basePath: List[String], file:Path): String = {
    val baseStr = basePath.mkString("/")
    val testStr = " " * (8 - baseStr.length) + baseStr + ": " + file.baseName
    testStr
  }

  /**
   * check if the test is a parse only test
   * @param basePath
   * @return true if the test is a parse only test
   */
  def isParseOnly(basePath: List[String]): Bool = {
    val parseOnly = basePath.headOption.contains("parser") || basePath.headOption.contains("compiler")
    parseOnly
  }


  /** Load type definitions and function definitions from a file into ctx
   * and declared definitions. This is useful for loading ocaml standard
   * library definitions
   */
  def loadLibrary(file: Path, typer: Typer, output: (String) => Any): (typer.Ctx, Map[Str, typer.PolymorphicType]) = {
    val fileContents = os.read(file)
    val allLines = fileContents.splitSane('\n').toIndexedSeq
    val block = OcamlParser.libraryTopLevelSeparators(allLines).mkString("\n")
    val fph = new FastParseHelpers(block)
    val globalStartLineNum = 0
    parse(block, p => new OcamlParser(Origin("builtin", globalStartLineNum, fph)).pgrm(p)
      , verboseFailures = true) match {
      case Failure(lbl, index, extra) =>
        val (lineNum, lineStr, col) = fph.getLineColAt(index)
        val globalLineNum = allLines.size + lineNum
        output("/!\\ Parse error: " + extra.trace().msg +
          s" at l.$globalLineNum:$col: $lineStr")
        output("Failed to parse library")
        (typer.Ctx.init, Map.empty)
      case Success(prog, index) => {
        val (typeDefs, stmts) = prog.desugared
        var ctx = typer.Ctx.init
        val raise: typer.Raise = d => ()
        var declared: Map[Str, typer.PolymorphicType] = Map.empty

        ctx = typer.processTypeDefs(typeDefs)(ctx, raise)
        val curBlockTypeDefs = ctx.tyDefs.iterator.map(_._2).toList
        typer.computeVariances(curBlockTypeDefs, ctx)

        stmts.foreach {
          // statement only declares a new term with its type
          // but does not give a body/definition to it
          case Def(_, nme, R(PolyType(tps, rhs)), _) =>
            val ty_sch = typer.PolymorphicType(0,
              typer.typeType(rhs)(ctx.nextLevel, raise,
                vars = tps.map(tp => tp.name -> typer.freshVar(typer.noProv/*FIXME*/)(1)).toMap))
            ctx += nme.name -> typer.VarSymbol(ty_sch, nme)
            declared += nme.name -> ty_sch

          // statement is defined and has a body/definition
          case d @ Def(isrec, nme, L(rhs), _) =>
            val ty_sch = typer.typeLetRhs(isrec, nme, rhs)(ctx, raise)
            // statement does not have a declared type for the body
            // the inferred type must be used and stored for lookup
            declared.get(nme.name) match {
              // statement has a body but it's type was not declared
              // infer it's type and store it for lookup and type gen
              case N =>
                ctx += nme.name -> typer.VarSymbol(ty_sch, nme)

              // statement has a body and a declared type
              // both are used to compute a subsumption (What is this??)
              // the inferred type is used to for ts type gen
              case S(sign) =>
                ctx += nme.name -> typer.VarSymbol(sign, nme)
                typer.uniState.subsume(ty_sch, sign)(ctx, raise, typer.TypeProvenance(d.toLoc, "def definition"))
            }

          case _ => ()
        }
        (ctx, declared)
      }
    }
  }

  /**
   * Fix the text output and remove the ascii art
   * @param output the output string
   * @return the fixed string
   */
  def fixText(output: Str): Str =
    output
      .replaceAll("╔══","")
      .replaceAll("╟── this", "This")
      .replaceAll("╟──", "")
      .replaceAll("║  ", "  ")

  /**
   * Output a message
   * @param info the message info
   * @param sctx the show context
   * @param output the output function
   * @param blockLineNum the block line number
   */
  def outputMsg(info: (Message, Ls[Loc], Bool, Int, Bool), sctx: ShowCtx,
                output: Str => Unit, blockLineNum: Int ): Unit = {
    val (msg, locs, dir, level, last) = info
    val levelOffset = " " * (level * 2)
    val msgPre = levelOffset ++ "◉ "
    val msgStr = msgPre ++ msg.showIn(sctx)
    output(msgStr)

    locs.zipWithIndex.foreach { case (loc, idx) =>
      var locPre = levelOffset ++ "│ "
      var termLinePre = levelOffset ++ "│ "
      if (last) locPre = levelOffset ++ "  "
      if (last) termLinePre = levelOffset ++ "  "

      val (startLineNum, _, startLineCol) = loc.origin.fph.getLineColAt(loc.spanStart)
      val (endLineNum, _, endLineCol) = loc.origin.fph.getLineColAt(loc.spanEnd)
      val lineNum = loc.origin.startLineNum + startLineNum - blockLineNum
      val lineNumPad = 5
      var lineNumStr = " " * lineNumPad // about the same space as if it had a 2 digit line number
      val lineBullet = " - "
      val truncateStr = " ..."

      // single line location and markers
      lineNumStr = if (loc.origin.fileName == "builtin") {
        "lib.".padTo(lineNumPad, ' ')
      } else {
        s"l.$lineNum".padTo(lineNumPad, ' ')
      }
      val fstLine = loc.origin.fph.lines(startLineNum - 1)
      if (!dir && idx == 0 && !last) termLinePre = levelOffset ++ "▲ "
      val linePre = termLinePre ++ lineBullet ++ lineNumStr
      output(linePre ++ fstLine)
      val gap = " " * (lineBullet + lineNumStr).length
      val offset = " " * (startLineCol - 1)

      if (endLineNum == startLineNum) {
        val markers = "^" * (endLineCol - startLineCol)
        output(locPre ++ gap ++ offset ++ markers)
      }
      // multi line location print first two lines
      // truncate if message runs past second line
      else {
        // markers for first line cover the line for multi line
        var markers = "^" * (fstLine.length - startLineCol + 1)
        output(locPre ++ gap ++ offset ++ markers)

        val truncate = endLineNum > (startLineNum + 1)
        var sndLine = loc.origin.fph.lines(startLineNum)
        if (truncate) sndLine ++= truncateStr
        val whitespace = sndLine.takeWhile(_ == ' ').length
        val linePre = " " * (lineBullet.length + lineNumStr.length)
        output(locPre ++ linePre ++ sndLine)

        val space = " " * (linePre.length + whitespace)
        markers = if (truncate) {
          "^" * (sndLine.length - whitespace)
        } else {
          "^" * (endLineCol - whitespace)
        }
        output(locPre ++ space ++ markers)
      }

      if (dir && idx == locs.length - 1 && !last) locPre = levelOffset ++ "▼ "
      if (idx == locs.length - 1 && !last) output(locPre)
    }
  }

  /**
   * Report a uni error
   * @param err the error report
   * @param output the output function
   * @param blockLineNum the block line number
   */
  def reportUniError(err: UniErrReport, output: Str => Unit, blockLineNum: Int): Unit = {
    val (mainMsg, seqStr, msgs, sctx, _, _) = UniErrReport.unapply(err).get

    if (err.level == 0) {
      val mainPre = "[ERROR] "
      output(s"$mainPre${mainMsg.showIn(sctx)}")
      if (seqStr.nonEmpty) {
        output("")
        output(" " * mainPre.length ++ seqStr)
      }
      output("")
    }

    msgs.zipWithIndex.foreach{
      case (L(msg), i) => outputMsg(msg, sctx, output, blockLineNum)
      case (R(report), _) => reportUniError(report, output, blockLineNum)
    }
  }


  /**
   * Check the test results
   * @param failures the failures
   * @param unmergedChanges the unmerged changes
   * @param beginTime the begin time
   * @param strw the results
   * @param testStr the test string
   * @param inParallel if the test is in parallel
   * @param file the file path
   * @param fileContents the file contents
   * @param fail the fail function
   */
  def checkTestResults(failures: Buffer[Int], unmergedChanges: Buffer[Int], beginTime: Long, strw: String, testStr: String,
                       inParallel: Boolean, file: Path, fileContents: String, fail: (String) => Unit): Unit = {

    val testFailed = failures.nonEmpty || unmergedChanges.nonEmpty
    val result = strw
    val endTime = System.nanoTime()
    val timeStr = (((endTime - beginTime) / 1000 / 100).toDouble / 10.0).toString
    val testColor = if (testFailed) Console.RED else Console.GREEN

    val resStr = s"${" " * (35 - testStr.length)}$testColor${
      " " * (6 - timeStr.length)}$timeStr  ms${Console.RESET}"

    if (inParallel) println(s"${Console.CYAN}Processed${Console.RESET}  $testStr$resStr")
    else println(resStr)

    if (result =/= fileContents) {
      println(s"! Updated $file")
      os.write.over(file, result)
    }

    if (testFailed)
      if (unmergedChanges.nonEmpty)
        fail(s"Unmerged non-output changes around: " + unmergedChanges.map("l."+_).mkString(", "))
      else fail(s"Unexpected diagnostics (or lack thereof) at: " + failures.map("l."+_).mkString(", "))

  }



}
