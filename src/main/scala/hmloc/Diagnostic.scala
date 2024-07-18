package hmloc

import hmloc.Diagnostic._
import hmloc.utils._
import hmloc.utils.shorthands._

import scala.collection.mutable

sealed abstract class Diagnostic(val theMsg: String) extends Exception(theMsg) {
  val allMsgs: Ls[Message -> Opt[Loc]]
  val kind: Kind
  val source: Source
}

object Diagnostic {

  sealed abstract class Kind
  case object Error extends Kind
  case object Warning extends Kind

  sealed abstract class Source
  case object Lexing extends Source
  case object Parsing extends Source
  case object Typing extends Source
  case object Compilation extends Source
  case object Runtime extends Source

}


final case class ErrorReport(mainMsg: Str, allMsgs: Ls[Message -> Opt[Loc]], source: Source) extends Diagnostic(mainMsg) {
  val kind: Kind = Error
}
object ErrorReport {
  def apply(msgs: Ls[Message -> Opt[Loc]], source: Source = Typing): ErrorReport =
    ErrorReport(msgs.head._1.show, msgs, source)
}

final case class WarningReport(mainMsg: Str, allMsgs: Ls[Message -> Opt[Loc]], source: Source) extends Diagnostic(mainMsg) {
  val kind: Kind = Warning
}
object WarningReport {
  def apply(msgs: Ls[Message -> Opt[Loc]], source: Source = Typing): WarningReport =
    WarningReport(msgs.head._1.show, msgs, source)
}

final case class UnificationReport(mainMsg: Str, allMsgs: Ls[Message -> Opt[Loc]], seqStr: Bool, source: Source = Typing) extends Diagnostic(mainMsg) {
  val kind: Kind = Error
}

object UnificationReport {
  def apply(msgs: Ls[Message -> Opt[Loc]], seqStr: Bool): UnificationReport =
    UnificationReport(msgs.head._1.show, msgs, seqStr)
}

final case class UniErrReport(mainMsg: Message, seqStr: Str, msgs: Ls[(Message, Ls[Loc], Bool, Int, Bool) \/ UniErrReport], showCtx: ShowCtx, level: Int = 0, source: Source = Typing) extends Diagnostic(mainMsg.toString) {
  override val allMsgs: Ls[(Message, Opt[Loc])] =
    (mainMsg -> N) :: msgs.flatMap {
      case L((msg, _, _, _, _)) => List(msg -> N)
      case R(report) => report.allMsgs
    }

  override val kind: Kind = Error
}

final case class Loc(spanStart: Int, spanEnd: Int, origin: Origin) {
  assert(spanStart >= 0)
  assert(spanEnd >= spanStart)

  def covers(that: Loc): Boolean = {
    that.origin == this.origin &&
    that.spanStart >= this.spanStart &&
    that.spanEnd <= this.spanEnd
  }

  def touches(that: Loc): Boolean = {
    that.origin == this.origin &&
    ((that.spanStart >= this.spanStart && that.spanStart <= this.spanEnd) ||
    (that.spanEnd <= this.spanEnd && that.spanEnd >= this.spanStart))
  }

  def merge(that: Loc): Loc = {
    require(this.origin == that.origin)
    Loc(this.spanStart min that.spanStart, this.spanEnd max that.spanEnd, origin)
  }

  def mergeOpt(that: Opt[Loc]): Loc = that.fold(this)(this.merge)

  def right: Loc = copy(spanStart = spanEnd)
  def left: Loc = copy(spanEnd = spanStart)

  def showLocationInSource: Str = {
    val (startLineNum, _, startLineCol) = origin.fph.getLineColAt(spanStart)
    val (endLineNum, _, endLineCol) = origin.fph.getLineColAt(spanEnd)
    val curLine = origin.fph.lines(startLineNum - 1)
    val lastCol = if (startLineNum == endLineNum) endLineCol else curLine.length
    val tickBuilder = new mutable.StringBuilder(curLine.substring(startLineCol - 1, lastCol))

    if (startLineNum != endLineNum) tickBuilder ++= " ..."

    tickBuilder.toString
  }
}

object Loc {
  implicit val LocOrdering: Ordering[Loc] = Ordering.by(loc => (loc.spanStart, loc.spanEnd))
}

final case class Origin(fileName: Str, startLineNum: Int, fph: FastParseHelpers) {
  override def toString: String = s"$fileName:+$startLineNum"

  def getLine(lineNum: Int): Option[String] =
    if (lineNum >= 1 && lineNum <= fph.lines.size) Some(fph.lines(lineNum - 1)) else None
}
