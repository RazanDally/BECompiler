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

import ModeDefaults._
import TestHelperFuncs._
import TestHelperConsts._

class DiffTests
  extends funsuite.AnyFunSuite
  with ParallelTestExecution
  with TimeLimitedTests
{

  val variableNamesUnderScore = mutable.Set[String]()
  val variableNamesParanthesis = mutable.Set[String]()

  private val inParallel = isInstanceOf[ParallelTestExecution]
  
  import DiffTests._

  // scala test will not execute a test if the test class has constructor parameters.
  // override this to get the correct paths of test files.
  protected lazy val files: Seq[Path] = allFiles.filter { file =>
    validExt(file.ext) && filter(file.relativeTo(pwd))
  }
  
  val timeLimit: Span = TimeLimit
  
  override val defaultTestSignaler: Signaler = new Signaler {
    @annotation.nowarn("msg=method stop in class Thread is deprecated") def apply(testThread: Thread): Unit = {
      println(s"!! Test at $testThread has run out out time !! stopping..." +
        "\n\tNote: you can increase this limit by changing DiffTests.TimeLimit")
      testThread.stop()
    }
  }

  def checkAndReplaceUnderscore(filePath: String): String = {
  // Read the file
  val lines = scala.io.Source.fromFile(filePath).getLines().toList

  // Collect all variable names in the file
  val variableNames = mutable.Set[String]()
  val variablePattern = """\blet\s+(\w+)""".r

  lines.foreach { line =>
    variablePattern.findAllIn(line).matchData.foreach(m => variableNames += m.group(1))
  }

  // Function to generate a new unique variable name
  def generateUniqueVarName(existingNames: mutable.Set[String]): String = {
    var newVarName = "newVar"
    var counter = 1
    // Continue generating names until a unique one is found
    while (existingNames.contains(newVarName)) {
      newVarName = s"newVar$counter"
      counter += 1
    }
    existingNames += newVarName
    newVarName
  }

  // Replace each 'let _' 'let()' with a unique variable name and save the variable names
  val modifiedLines = lines.map { line =>
  if (line.contains("let _")) {
    val uniqueVarName = generateUniqueVarName(variableNames)
    variableNamesUnderScore += uniqueVarName
    line.replaceAll("""\blet\s+_""", s"let $uniqueVarName")
  } else if (line.contains("let ()")) {
    val uniqueVarName = generateUniqueVarName(variableNames)
    variableNamesParanthesis += uniqueVarName
    line.replaceAll("""\blet\s+\(\)""", s"let $uniqueVarName")
  } else {
    line
  }
}

    return modifiedLines.mkString("\n")

}

  files.foreach { file =>
        val basePath = file.segments.drop(dir.segmentCount).toList.init
        val testName = basePath.map(_ + "/").mkString + file.baseName
        test(testName) {

    val testStr = getTestStr(basePath, file)

    if (!inParallel) print(s"Processing $testStr")

    // * For some reason, the color is sometimes wiped out when the line is later updated not in iTerm3:
    // if (!inParallel) print(s"${Console.CYAN}Processing${Console.RESET} $testStr ... ")

    val beginTime = System.nanoTime()
    var separator = "\r\n"
    val fileContents = checkAndReplaceUnderscore(file.toString)

    if(!fileContents.contains('\r')){
      separator = "\n"
    }
    val allLines = fileContents.split(separator).toList
    val strw = new java.io.StringWriter
    val out = new java.io.PrintWriter(strw) {
      override def println(): Unit = print(separator) // to avoid inserting CRLF on Windows
    }
    var stdout = false
    def output(str: String) =
      // out.println(outputMarker + str)
      if (stdout) System.out.println(str) else
      str.split(separator).foreach(l => out.println(outputMarker + l))
    def reportOutput(str: String) = str.split(separator).foreach(l => out.println(outputMarker + l))
    val typer = new Typer(dbg = false, verbose = false, explainErrors = false) {
      override def emitDbg(str: String): Unit = output(str)
    }
    var ctx: typer.Ctx = typer.Ctx.init
    var declared: Map[Str, typer.PolymorphicType] = Map.empty
    val failures = mutable.Buffer.empty[Int]
    val unmergedChanges = mutable.Buffer.empty[Int]

    val defaultMode = Mode()

    var parseOnly = isParseOnly(basePath)

    def rec(lines: List[String], mode: Mode): Unit = lines match {
      case "" :: Nil =>
      case line :: ls if line.startsWith(":") =>
        out.println(line)
        // process command line
        val command = line.tail.takeWhile(!_.isWhitespace)
        // check normal command actions setup in Mode Defaults
        val newMode = modeActionMap.getOrElse(command, { _: Mode =>
          //if none of them matched, check these cases
          command match{
            case "ParseOnly" => parseOnly = true; mode
            case "AllowTypeErrors" => allowTypeErrors = true; mode
            case "AllowParseErrors" => allowParseErrors = true; mode
            case "AllowRuntimeErrors" => allowRuntimeErrors = true; mode
            case "ShowRelativeLineNums" => showRelativeLineNums = true; mode
            case "NoProvs" => noProvs = true; mode
            // Parse and check the file with ocaml syntax and semantic rules
            case "OcamlParser" => ocamlMode = true; mode
            // don't load ocaml library in special cases
            case "NoLibrary" =>
              ocamlLoadLibrary = false
              ctx = typer.Ctx.init
              declared = Map.empty
              mode
            // load ocaml library definitions and use the updated context and declarations
            case "OcamlLoadLibrary" =>
              ocamlLoadLibrary = true
              val (libCtx, libDeclared): (typer.Ctx, Map[Str, typer.PolymorphicType]) = loadLibrary(DiffTests.libPath, typer, output(_))
              ctx = libCtx
              declared = libDeclared
              mode
            case _ =>
              failures += allLines.size - lines.size
              output("/!\\ Unrecognized option " + line)
              mode          }
        })(mode)


        rec(ls, newMode)
      case line :: ls if line.startsWith("// FIXME") || line.startsWith("// TODO") =>
        out.println(line)
        rec(ls, mode.copy(fixme = true))
      case line :: ls if line.startsWith(outputMarker)
        => rec(ls, defaultMode)
      case line :: ls if line.isEmpty =>
        out.println(line)
        rec(ls, defaultMode)
      case line :: ls if line.startsWith("//") =>
        out.println(line)
        rec(ls, mode)

      // process block of text and show output - type, expressions, errors
      case l :: ls =>
        val blockLineNum = (allLines.size - lines.size) + 1

        val block = (l :: ls.takeWhile(l => l.nonEmpty && !(
          l.startsWith(outputMarker)
        ))).toIndexedSeq
        block.foreach(out.println)
        val processedBlock = OcamlParser.addTopLevelSeparators(block)
        val processedBlockStr = processedBlock.mkString
        val fph = new FastParseHelpers(block)
        val globalStartLineNum = allLines.size - lines.size + 1

        var totalTypeErrors = 0
        var totalParseErrors = 0
        var totalWarnings = 0

        def report(diags: Ls[hmloc.Diagnostic], output: Str => Unit = reportOutput): Unit =
          if (mode.tex) reportBase(diags, str => output(fixText(str))) else reportBase(diags, output)


        // report errors and warnings
        def reportBase(diags: Ls[hmloc.Diagnostic], output: Str => Unit): Unit = {
          diags.foreach {
            case report: UniErrReport => reportUniError(report, output, blockLineNum)
            case diag =>
            var unificationRelativeLineNums = false
            val sctx = Message.mkCtx(diag.allMsgs.iterator.map(_._1), "?")
            val headStr = diag match {
              case ErrorReport(_, loco, src) =>
                src match {
                  case Diagnostic.Lexing =>
                    totalParseErrors += 1
                    lexicalErrorText
                  case Diagnostic.Parsing =>
                    totalParseErrors += 1
                    parseErrorText
                  case _ => // TODO customize too
                    totalTypeErrors += 1
                    basicErrorText
                }
              case _: UnificationReport =>
                totalTypeErrors += 1
                basicErrorText
              case WarningReport(msg, loco, _) =>
                totalWarnings += 1
                warningText
            }
            val seqStr = diag match {
              case UnificationReport(_, _, seqStr, _) =>
                unificationRelativeLineNums = true
                seqStr
              case _ => false
            }

            val lastMsgNum = diag.allMsgs.size - 1
            var globalLineNum = blockLineNum  // solely used for reporting useful test failure messages
            val tex = mode.tex
            diag.allMsgs.zipWithIndex.foreach { case ((msg, loco), msgNum) =>
              val isLast = msgNum =:= lastMsgNum
              val msgStr = msg.showIn(sctx)
              if (msgNum =:= 0 && !tex) {
                output(headStr + msgStr)
                output(prepre)
              }
              else if (msgNum =:= 0) {
                output(headStr + msgStr)
              }
              // unification error has seq string
              else if (msgNum =:= 1 && seqStr) {
                output(s"╟── $msgStr")
                output(prepre)
              }
              else output(s"${if (isLast && loco.isEmpty) "╙──" else "╟──"} $msgStr")
              if (loco.isEmpty && diag.allMsgs.size =:= 1) output("╙──")
              loco.foreach { loc =>
                val (startLineNum, _, startLineCol) =
                  loc.origin.fph.getLineColAt(loc.spanStart)
                if (globalLineNum =:= 0) globalLineNum += startLineNum - 1
                val (endLineNum, _, endLineCol) =
                  loc.origin.fph.getLineColAt(loc.spanEnd)
                var l = startLineNum
                var c = startLineCol
                while (l <= endLineNum) {
                  val globalLineNum = loc.origin.startLineNum + l - 1
                  val relativeLineNum = globalLineNum - blockLineNum + 1
                  val lineNumber = {
                    if (loc.origin.fileName == "builtin") "builtin:"
                    else if (l == startLineNum) {
                      val linemarker = "l."
                      if (unificationRelativeLineNums || showRelativeLineNums && relativeLineNum > 0) s"$linemarker$relativeLineNum:"
                      else s"$linemarker$globalLineNum:"
                    } else "    " // about the same space as if it had line number
                  }
                  if (tex) {
                    val curLine = loc.origin.fph.lines(l - 1)
                    val whitespace = curLine.takeWhile(c => c == ' ').length
                    // println(curLine.length.toString)
                    // println(stdout)
                    // output(prepre + pre + "\t" + curLine)
                    val tickBuilder = new mutable.StringBuilder()
                    // tickBuilder ++= (prepre + " " * pre.length + "\t" + " " * (c - 1))
                    val lastCol = if (l =:= endLineNum) endLineCol min (curLine.length + 1)
                      else curLine.length + 1
                    // println(lastCol,curLine.length)
                    var i = 0
                    while (i < c - 1) { tickBuilder += curLine(i); i += 1 }
                    while (c <= whitespace) { tickBuilder += ' '; c += 1 }
                    tickBuilder ++= "_B_"
                    while (c < lastCol) { tickBuilder += curLine(c - 1); c += 1 }
                    if (l - startLineNum =:= 1 && l != endLineNum) {
                      tickBuilder ++= " ..."
                      l = endLineNum + 1
                    }
                    tickBuilder ++= "__"
                    while (c <= curLine.length) { tickBuilder += curLine(c - 1); c += 1 }
                    // truncate large output second line onwards
                    val lnStr = tickBuilder.toString
                      .replaceAll("let", "**let**")
                      .replaceAll("val", "**val**")
                    output(prepre + lineNumber + "‹   " + lnStr + "›")
                  } else {
                    val curLine = loc.origin.fph.lines(l - 1)
                    val whitespace = curLine.takeWhile(c => c == ' ').length
                    var dotextend = false
                    // truncate large output second line onwards
                    if (l - startLineNum =:= 1 && l != endLineNum) {
                      dotextend = true
                      output(prepre + lineNumber + "\t" + curLine + " ...")
                      l = endLineNum + 1
                    } else {
                      output(prepre + lineNumber + "\t" + curLine)
                    }
                    val tickBuilder = new mutable.StringBuilder()
                    tickBuilder ++= (
                      (if (isLast && l =:= endLineNum) "╙──" else prepre)
                      + " " * lineNumber.length + "\t" + " " * (c - 1))
                    val lastCol = if (l =:= endLineNum) endLineCol else curLine.length + 1
                    while (c < lastCol) {
                      if (c > whitespace) tickBuilder += '^'
                      else tickBuilder += ' '
                      c += 1
                    }
                    if (c =:= startLineCol) tickBuilder += '^'
                    if (dotextend) tickBuilder ++= "^^^^"
                    output(tickBuilder.toString)
                  }
                  c = 1
                  l += 1
                }
              }
            }
            if (diag.allMsgs.isEmpty) output("╙──")

            if (!mode.fixme) {
              if (!allowTypeErrors
                  && !mode.expectTypeErrors && diag.isInstanceOf[ErrorReport] && diag.source =:= Diagnostic.Typing)
                failures += globalLineNum
              if (!allowParseErrors
                  && !mode.expectParseErrors && diag.isInstanceOf[ErrorReport] && (diag.source =:= Diagnostic.Lexing || diag.source =:= Diagnostic.Parsing))
                failures += globalLineNum
              if (!allowTypeErrors && !allowParseErrors
                  && !mode.expectWarnings && diag.isInstanceOf[WarningReport])
                failures += globalLineNum
            }

            ()
          }
        }

        val raise: typer.Raise = d => report(d :: Nil)

        try {
          parse(processedBlockStr, p => new OcamlParser(Origin(testName, globalStartLineNum, fph)).pgrm(p) )
        } match {
          case f: Failure =>
            val Failure(_, index, extra) = f
            val (lineNum, lineStr, col) = fph.getLineColAt(index)
            val globalLineNum = (allLines.size - lines.size) + lineNum
            if (!mode.expectParseErrors && !mode.fixme)
              failures += globalLineNum
            output("/!\\ Parse error: " + extra.trace().msg +
              s" at l.$globalLineNum:$col: $lineStr")

          // successfully parsed block into a valid syntactically valid program
          case Success(prog, _) =>
            if (mode.expectParseErrors)
              failures += blockLineNum
            if (mode.dbgParsing) output(s"Parsed: ${PrettyPrintHelper.inspect(prog)}")
            if (mode.stats) typer.resetStats()
            typer.dbg = mode.dbg
            typer.unifyMode = mode.unify

            typer.verbose = mode.verbose
            typer.explainErrors = mode.explainErrors
            // survey programs should only output diagnostics
            stdout = mode.stdout || file.baseName.contains("Survey")

            // In parseOnly mode file only parse and print desugared blocks for file
            // do not perform type checking or codegen or results
            val (_, (typeDefs, stmts)) = if (parseOnly) {
              val (typeDefs, stmts) = prog.desugared
              typeDefs.foreach(td => output("Desugared: " + td))
              stmts.foreach { s =>
                output("Desugared: " + s)
              }
              (Pgrm(Nil), (Nil, Nil))
            } else {
              (prog, prog.desugared)
            }

            val oldCtx = ctx
            ctx =
              typer.processTypeDefs(typeDefs)(ctx, raise)

            def getType(ty: typer.TypeScheme): Type = {
              val wty = ty.uninstantiatedBody
              if (mode.isDebugging) output(s"⬤ U Typed as: $wty")
              if (mode.isDebugging) output(s" where: ${wty.showUnified}")
              typer.dbg = mode.dbgSimplif

              val exp = if (mode.noSimplification) typer.expandUnifiedType(wty)(ctx)
              else {
                object SimplifyPipeline extends typer.SimplifyPipeline {
                  def debugOutput(msg: => Str): Unit =
                    if (mode.dbgSimplif) output(msg)
                }
                val sim = SimplifyPipeline(wty)(ctx)
                val exp = typer.expandUnifiedType(sim)(ctx)
                if (mode.dbgSimplif) output(s"⬤ Expanded: $exp")
                exp
              }

              exp
            }

            val curBlockTypeDefs = typeDefs
              // add check from process type def block below
              .flatMap(td => if (!oldCtx.tyDefs.contains(td.nme.name)) ctx.tyDefs.get(td.nme.name) else None)

            // activate typer tracing if variance debugging is on and then set it back
            // this makes it possible to debug variance in isolation
            val temp = typer.dbg
            typer.dbg = mode.debugVariance
            typer.computeVariances(curBlockTypeDefs, ctx)
            typer.dbg = temp
            val varianceWarnings: MutMap[TypeName, Ls[TypeName]] = MutMap()

            // show the result of type inference for each processed type def
            typeDefs.foreach(td =>
              // check if type def is not previously defined
              if (ctx.tyDefs.contains(td.nme.name)
                  && !oldCtx.tyDefs.contains(td.nme.name)) {
                  // ^ it may not end up being defined if there's an error

                val tn = td.nme.name
                val ttd = ctx.tyDefs(tn)
                val tvv = ttd.tvarVariances.getOrElse(die)

                // generate warnings for bivariant type variables
                val bivariantTypeVars = ttd.tparamsargs.iterator.filter{ case (_, tvar) =>
                  tvv.get(tvar).contains(typer.VarianceInfo.bi)
                }.map(_._1).toList
                if (bivariantTypeVars.nonEmpty) {
                  varianceWarnings.put(td.nme, bivariantTypeVars)
                }

                val params = if (ttd.tparamsargs.nonEmpty)
                    SourceCode.horizontalArray(ttd.tparamsargs.map{ case (tname, tvar) =>
                      val tvarVariance = tvv.getOrElse(tvar, throw new Exception(
                        s"Type variable $tvar not found in variance store ${ttd.tvarVariances} for $ttd"))
                      SourceCode(s"${tvarVariance.show}${tname.name}")
                    })
                  else
                    SourceCode("")
                output(s"Defined " + td.kind.str + " " + tn + params)
              }
            )

            if (varianceWarnings.nonEmpty) {
              import Message._
              val diags = varianceWarnings.iterator.map { case (tname, biVars) =>
                val warnings = biVars.map( tname => msg"${tname.name} is irrelevant and may be removed" -> tname.toLoc)
                WarningReport(msg"Type definition ${tname.name} has bivariant type parameters:" -> tname.toLoc :: warnings)
              }.toList
              report(diags)
            }

            // L(diagnostic lines) | R(binding name -> typing output lines)
            val typingOutputs = mutable.Buffer.empty[Ls[Str] \/ (Str -> Ls[Str])]

            val diagLineBuffers = mutable.Buffer.empty[Str]
            val raiseToBuffer: typer.Raise = d => {
              report(d :: Nil, diagLineBuffers += _)
              typingOutputs += L(diagLineBuffers.toList)
              diagLineBuffers.clear()
            }

            // process statements and output types
            // all `Def`s and `Term`s are processed here
            stmts.foreach {
              // statement only declares a new term with its type
              // but does not give a body/definition to it
              case Def(isrec, nme, R(PolyType(tps, rhs)), _) =>
                typer.dbg = mode.dbg
                val ty_sch = typer.PolymorphicType(0,
                  typer.typeType(rhs)(ctx.nextLevel, raiseToBuffer,
                    vars = tps.map(tp => tp.name -> typer.freshVar(typer.noProv/*FIXME*/)(1)).toMap))
                ctx += nme.name -> typer.VarSymbol(ty_sch, nme)
                declared += nme.name -> ty_sch
                val exp = getType(ty_sch)
                typingOutputs += R[Ls[Str], Str -> Ls[Str]](nme.name -> (s"$nme: ${exp.show}" :: Nil))

              // statement is defined and has a body/definition
              case d @ Def(isrec, nme, L(rhs), _) =>
                typer.dbg = mode.dbg
                val ty_sch = typer.typeLetRhs(isrec, nme, rhs)(ctx, raiseToBuffer)
                val exp = getType(ty_sch)
                // statement does not have a declared type for the body
                // the inferred type must be used and stored for lookup
                val typingOutput = declared.get(nme.name) match {
                  // statement has a body but it's type was not declared
                  // infer it's type and store it for lookup and type gen
                  case N =>
                    ctx += nme.name -> typer.VarSymbol(ty_sch, nme)
                    s"$nme: ${exp.show}" :: Nil

                  // statement has a body and a declared type
                  // both are used to compute a subsumption (What is this??)
                  // the inferred type is used to for ts type gen
                  case S(sign) =>
                    ctx += nme.name -> typer.VarSymbol(sign, nme)
                    val sign_exp = getType(sign)
                    typer.dbg = mode.dbg
                    typer.uniState.subsume(ty_sch, sign)(ctx, raiseToBuffer, typer.TypeProvenance(d.toLoc, "def definition"))
                    exp.show :: s"  <:  $nme:" :: sign_exp.show :: Nil
                }
                typingOutputs += R[Ls[Str], Str -> Ls[Str]](nme.name -> typingOutput)
              case desug: Statement =>
                typer.dbg = mode.dbg
                typer.typeStatement(desug, allowPure = true)(ctx, raiseToBuffer) match {
                  // when does this happen??
                  case R(binds) =>
                    binds.foreach { case (nme, pty) =>
                      val ptType = getType(pty)
                      ctx += nme -> typer.VarSymbol(pty, Var(nme))
                      typingOutputs += R[Ls[Str], Str -> Ls[Str]](nme -> (s"$nme: ${ptType.show}" :: Nil))
                    }

                  // statements for terms that compute to a value
                  // and are not bound to a variable name
                  case L(pty) =>
                    val exp = getType(pty)
                    if (exp =/= TypeName("unit")) {
                      val res = "res"
                      ctx += res -> typer.VarSymbol(pty, Var(res))
                      typingOutputs += R[Ls[Str], Str -> Ls[Str]](res, s"res: ${exp.show}" :: Nil)
                    } else typingOutputs += R[Ls[Str], Str -> Ls[Str]]("" -> Nil)
                }
            }

            // generate unification for the type variables created in the current
            // typing unit.
            if (mode.unify) {
              val temp = typer.dbg
              typer.dbg = mode.unifyDbg
              typer.uniState.unify()
              val errors = typer.printUnificationErrors()
              if (errors.nonEmpty) typingOutputs += L(errors)
              typer.reportNewUnificationErrors(ctx, raise)
              typer.uniState.clear()
              typer.dbg = temp
            }

            // Print type checking results
            typingOutputs.foreach {
              case L(diagnosticLines) => diagnosticLines.foreach(reportOutput)
              case R(_ -> typingResults) => typingResults.foreach(output)
            }

            if (mode.stats) {
              val (co, an, su, _) = typer.stats
              output(s"constrain calls  : " + co)
              output(s"annoying  calls  : " + an)
              output(s"subtyping calls  : " + su)
              // output(s"constructed types: " + ty)
            }

            if (mode.expectParseErrors && totalParseErrors =:= 0)
              failures += blockLineNum
            if (mode.expectTypeErrors && totalTypeErrors =:= 0)
              failures += blockLineNum
            if (mode.expectWarnings && totalWarnings =:= 0)
              failures += blockLineNum
        } catch {
          case oh_noes: ThreadDeath => throw oh_noes
          case err: Throwable =>
            if (!mode.fixme)
              failures += allLines.size - lines.size
            // err.printStackTrace(out)
            output("/!!!\\ Uncaught error: " + err +
              err.getStackTrace.take(
                if (mode.fullExceptionStack) Int.MaxValue
                else if (mode.fixme) 0
                else 10
              ).map("\n" + "\tat: " + _).mkString)
        } finally {
          typer.dbg = false
          typer.verbose = false
        }
        rec(lines.drop(block.size), mode)
      case Nil =>
    }

    // load ocaml library by default
    ocamlLoadLibrary = true
    val (libCtx, libDeclared): (typer.Ctx, Map[Str, typer.PolymorphicType]) = loadLibrary(DiffTests.libPath, typer, output(_))
    ctx = libCtx
    declared = libDeclared

    try rec(allLines, defaultMode) finally {
      out.close()
    }
     // replace the variable names back to _ and () after running the file
    var result = strw.toString
    val lines = result.split(separator).toList
    val modifiedLines = lines.map { line =>
      var modifiedLine = line
      if (variableNamesUnderScore.nonEmpty) {
        variableNamesUnderScore.foreach { name =>
          modifiedLine = modifiedLine.replaceAll(s"\\blet\\s+$name\\b", "let _")
          modifiedLine = modifiedLine.replaceAll(name, "- ")

        }
      }
      if (variableNamesParanthesis.nonEmpty) {
        variableNamesParanthesis.foreach { name =>
          modifiedLine = modifiedLine.replaceAll(s"\\blet\\s+$name\\b", "let ()")
          modifiedLine = modifiedLine.replaceAll(name, "- ")

        }
      }
      modifiedLine
    }
    result = modifiedLines.mkString(separator)
    
    checkTestResults(failures, unmergedChanges, beginTime, result, testStr,
      inParallel,file, fileContents, fail(_))
  }}
}

object DiffTests {
  
  private val TimeLimit =
    if (sys.env.contains("CI")) Span(25, Seconds)
    else Span(10000, Seconds)
  
  private val pwd = os.pwd
  private val dir = pwd/"src"/"test"/"diff"
  private val libPath = dir/"ocaml"/"OcamlLibrary.mls"

  private val allFiles = os.walk(dir).filter(_.toIO.isFile)

  private val validExt = Set("ml", "mls")
  
  // Aggregate unstaged modified files to only run the tests on them, if there are any
  private val modified: Set[os.RelPath] =
    try os.proc("git", "status", "--porcelain", dir).call().out.lines().iterator.flatMap { gitStr =>
      println(" [git] " + gitStr)
      val prefix = gitStr.take(2)
      val filePath = os.RelPath(gitStr.drop(3))
      if (prefix =:= "A " || prefix =:= "M ") N else S(filePath) // disregard modified files that are staged
    }.toSet catch {
      case err: Throwable => System.err.println("/!\\ git command failed with: " + err)
      Set.empty
    }
  
  def filter(file: os.RelPath): Bool = modified(file) || modified.isEmpty
}
