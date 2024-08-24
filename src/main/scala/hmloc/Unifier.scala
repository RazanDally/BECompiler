package hmloc

import hmloc.Message.MessageContext
import hmloc.utils._
import hmloc.utils.shorthands._

import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.collection.mutable.{Queue => MutQueue, Set => MutSet, Map => MutMap}

/* Unification solver creates data flows from sub-typing constraints. It also formats
 * reports for incorrect data flows.
 * 
 * The unification algorithm is formally described in the section 4 of the paper.
 */
trait Unifier extends TyperDatatypes with UnifierHelpers {
  self: Typer =>
  val cache: MutSet[(ST, ST)] = MutSet()
  var unifyMode: Bool = false

  val uniState = new UState()

  def printUnificationErrors(): Ls[Str] = {
    if (uniState.error.nonEmpty) {
      uniState.reportStats :: s"UERR ${uniState.error.size} errors" :: uniState.error.map(_.toString()).toList
    } else {
      Ls()
    }
  }

  def reportNewUnificationErrors(implicit ctx: Ctx, raise: Raise): Unit =
    uniState.error.toList.sorted.foreach(u => raise(u.createErrorMessage()(ctx, u.sequenceTVs)))

  case class Unification(flow: Queue[DataFlow]) extends Ordered[Unification] {
    lazy val a: ST = flow.head.getStart
    lazy val b: ST = flow.last.getEnd
    lazy val level: Int = flow.iterator.map(_.level).max
    lazy val sequenceTVs: Set[TV] = {
      val tvSet: MutSet[TV] = MutSet()
      constraintSequence.map { case (Constraint(a, b), _) =>
        a.unwrapProvs match { case tv: TypeVariable => tvSet += tv case _ => ()}
        b.unwrapProvs match { case tv: TypeVariable => tvSet += tv case _ => ()}
      }
      tvSet.toSet
    }

    override def compare(that: Unification): Int = {
      val levelComp = this.level.compare(that.level)
      if (levelComp != 0) {
        return levelComp
      }
      val lengthComp = this.constraintSequence.length.compare(that.constraintSequence.length)
      if (lengthComp != 0) {
        return lengthComp
      }
      val aComp = a.ord.compare(a, that.a)
      if (aComp == 0) {
        return b.ord.compare(b, that.b)
      }
      aComp
    }

    override def toString: Str = s"L: $level [${a.unwrapProvs} ~ ${b.unwrapProvs}, ${flow.mkString(", ")}]"

    private def createCtx(implicit ctx: Ctx): ShowCtx =
      ShowCtx.mk(sequenceTVs.map(_.expOcamlTy()(ctx, sequenceTVs)), "?")

    private lazy val constraintSequence: Ls[(Constraint, Int)] = flow.iterator.flatMap {
      case c: Constraint => (c, level) :: Nil
      case Ctor_Uni(a, b, ctora, ctorb, uni) =>
        (Constraint.startTransition(a, ctora), level) :: uni.constraintSequence ::: (Constraint.endTransition(ctorb, b), level) :: Nil
    }.toList

    private def generateSequenceString(implicit ctx: Ctx): Str = {
      implicit val showTV: Set[TV] = sequenceTVs

      // Currently only show type sequence for types at the current level of nesting
      val sequenceMessages: List[Message] = constraintSequence.iterator.zipWithIndex.collect {
        case ((c @ Constraint(a, b), lvl), idx) if lvl == level =>
          c.transition match {
            case Some(true) =>
              msg"(${a.expOcamlTy()(ctx, showTV)}) ~~~~ "
            case Some(false) =>
              if (idx == constraintSequence.length - 1)
                msg"(${b.expOcamlTy()(ctx, showTV)})"
              else
                msg""
            case None =>
              val arrowStr = if (c.dir) "--->" else "<---"
              val last = idx == constraintSequence.length - 1
              msg"(${a.expOcamlTy()(ctx, showTV)}) $arrowStr " + {
                if (last) msg"(${b.expOcamlTy()(ctx, showTV)})" else msg""
              }
          }
      }.toList

      val sctx = createCtx
      val sb = new mutable.StringBuilder()
      sequenceMessages.foreach(msg => sb ++= msg.showIn(sctx))
      sb.toString()
    }


    def concat(other: Unification): Unification = {
      assert(b.unwrapProvs == other.a.unwrapProvs, s"$b != ${other.a}")
      Unification(flow.enqueueAll(other.flow))
    }

    private def nestingLevel: Int = flow.map {
      case _: Constraint => 0
      case ctor: Ctor_Uni => 1 + ctor.uni.nestingLevel
    }.max

    def rev: Unification = Unification(flow.map(_.rev).reverse)

    def createErrorMessage(level: Int = 0, showCtx: Opt[ShowCtx] = N)(implicit ctx: Ctx, showTV: Set[TV]): UniErrReport = {
      println(s"UERR REPORT $toString")
      val sctx = showCtx.getOrElse(createCtx)
      val mainMsg = msg"Type `${a.expOcamlTy()(ctx, Set())}` does not match `${b.expOcamlTy()(ctx, Set())}`"
      val seqString = generateSequenceString
      def msg(a: ST): Message = a.unwrapProvs match {
        case tv: TV => msg"(${tv.expOcamlTy()}) is assumed for"
        case st => msg"(${st.expOcamlTy()}) comes from"
      }

      def constraintToMessage(c: Constraint, last: Bool = false): Ls[(Message, Ls[Loc], Bool, Int, Bool)] = {
        val (a, b) = Constraint.unapply(c).get
        val flow = c.dir
        val locs = extractLocations(c)

        if (last) {
          handleLastConstraint(a, b, locs, flow)
        } else {
          createMessage(a, locs, flow, level, isLast = false) :: Nil
        }
      }

      def extractLocations(c: Constraint): Ls[Loc] = {
        c.getCleanProvs.collect {
          case TypeProvenance(S(loc), _, _, _) => loc
        }
      }

      def handleLastConstraint(a: ST, b: ST, locs: Ls[Loc], flow: Bool): Ls[(Message, Ls[Loc], Bool, Int, Bool)] = {
        if (locs.isEmpty) {
          throw new Exception("No locs for relation")
        } else if (locs.length == 1) {
          createMessage(a, locs, flow, level, isLast = false) :: createMessage(b, locs, flow, level, isLast = true) :: Nil
        } else {
          createMessage(a, locs.init, flow, level, isLast = false) :: createMessage(b, locs.last :: Nil, flow, level, isLast = true) :: Nil
        }
      }

      def createMessage(ty: ST, locs: Ls[Loc], flow: Bool, level: Int, isLast: Bool): (Message, Ls[Loc], Bool, Int, Bool) = {
        (msg(ty), locs, flow, level, isLast)
      }


      // Helpful show types being projected from their constructors
      def constructorArgumentMessage(c: Ctor_Uni, leftEnd: Bool, level: Int): (Message, Ls[Loc], Bool, Int, Bool) = {
        val ty = selectType(c, leftEnd)
        val locs = extractUniqueTypeUseLocations(ty)
        createMessage(ty, locs, false, level, true)
      }

      def selectType(c: Ctor_Uni, leftEnd: Bool): ST = {
        if (leftEnd) c.a else c.b
      }

      def extractUniqueTypeUseLocations(ty: ST): Ls[Loc] = {
        ty.uniqueTypeUseLocations.collect {
          case TypeProvenance(S(loc), _, _, _) => loc
        }
      }


      val report = {
        val msgs = flow.iterator.sliding(2).zipWithIndex.collect {
          case (Seq(c: Constraint), _) =>
            constraintToMessage(c, true).map(L(_))

          case (Seq(ctor@Ctor_Uni(_, _, _, _, uni)), _) =>
            List(
              L(constructorArgumentMessage(ctor, true, level)),
              R(uni.createErrorMessage(level + 1, S(sctx))(ctx, showTV)),
              L(constructorArgumentMessage(ctor, false, level))
            )

          case (Seq(c: Constraint, _: Constraint), _) =>
            constraintToMessage(c).map(L(_))

          case (Seq(c: Constraint, _: Ctor_Uni), _) =>
            constraintToMessage(c, true).map(L(_))

          case (Seq(ctor@Ctor_Uni(_, _, _, _, uni), ctor2: Ctor_Uni), idx) if ctor.b === ctor2.a =>
            val nestedReport = R(uni.createErrorMessage(level + 1, S(sctx))(ctx, showTV))
            val project = L(constructorArgumentMessage(ctor, false, level))
            if (idx == 0) {
              List(
                L(constructorArgumentMessage(ctor, true, level)),
                nestedReport,
                project
              )
            } else {
              List(nestedReport, project)
            }

          case (Seq(ctor@Ctor_Uni(_, _, _, _, uni), _), idx) =>
            val nestedReport = List(R(uni.createErrorMessage(level + 1, S(sctx))(ctx, showTV)))
            if (idx == 0) {
              L(constructorArgumentMessage(ctor, true, level)) :: nestedReport
            } else {
              nestedReport
            }
        }.flatten.toList ::: {
          if (flow.length != 1) {
            flow.last match {
              case c: Constraint =>
                constraintToMessage(c, true).map(L(_))

              case ctor@Ctor_Uni(_, _, _, _, uni) =>
                List(
                  R(uni.createErrorMessage(level + 1, S(sctx))(ctx, showTV)),
                  L(constructorArgumentMessage(ctor, false, level))
                )
            }
          } else {
            Nil
          }
        }

        UniErrReport(mainMsg, seqString, msgs, sctx, level)
      }
      report
    }
  }

  object Unification {
    def fromLhsRhs(lhs: ST, rhs: ST): Unification = Unification(Queue(Constraint(lhs, rhs)))
  }
  // Note: maybe this and `extrude` should be merged?
  def freshenAbove(lim: Int, ty: SimpleType, rigidify: Bool = false)(implicit lvl: Int): SimpleType = {
    val freshened = MutMap.empty[TV, SimpleType]

    def freshenDataFlow(df: DataFlow): DataFlow = df match {
      case c@Constraint(a, b) =>
        val c1 = Constraint(freshen(a), freshen(b))
        c1.dir = c.dir
        c1
      case Ctor_Uni(a, b, ctora, ctorb, uni) =>
        Ctor_Uni(freshen(a), freshen(b), freshen(ctora), freshen(ctorb), freshenUnification(uni))
    }

    def freshenUnification(u: Unification): Unification = u.copy(flow = u.flow.map(freshenDataFlow))

    def freshen(ty: SimpleType): SimpleType = {
      if (!rigidify && ty.level <= lim) ty else ty match {
        case tv: TypeVariable => freshenTypeVariable(tv)
        case t@FunctionType(l, r) => FunctionType(freshen(l), freshen(r))(t.prov)
        case t@ComposedType(p, l, r) => ComposedType(p, freshen(l), freshen(r))(t.prov)
        case t@TupleType(fs) => TupleType(fs.mapValues(freshen))(t.prov)
        case e@ExtrType(_) => e
        case p@ProvType(und) => ProvType(freshen(und))(p.prov)
        case _: RigidTypeVariable => ty
        case tr@TypeRef(d, ts) => TypeRef(d, ts.map(freshen))(tr.prov)
      }
    }

    def freshenTypeVariable(tv: TypeVariable): SimpleType = {
      freshened.get(tv) match {
        case Some(tv) => tv
        case None if rigidify => createRigidTypeVariable(tv)
        case None => createFreshTypeVariable(tv)
      }
    }

    def createRigidTypeVariable(tv: TypeVariable): SimpleType = {
      val rv = RigidTypeVariable(Var(tv.nameHint.getOrElse("_" + freshVar(noProv).toString)))(tv.prov)
      if (tv.lowerBounds.nonEmpty || tv.upperBounds.nonEmpty) {
        val tv2 = freshVar(tv.prov, tv.nameHint)
        freshened += tv -> tv2
        tv2.lowerBounds ::= tv.lowerBounds.map(freshen).foldLeft(rv: ST)(_ & _)
        tv2.upperBounds ::= tv.upperBounds.map(freshen).foldLeft(rv: ST)(_ | _)
        tv2.uni = tv.uni.map(freshenUnification)
        tv2
      } else {
        freshened += tv -> rv
        rv
      }
    }

    def createFreshTypeVariable(tv: TypeVariable): SimpleType = {
      val v = freshVar(tv.prov, tv.nameHint)
      freshened += tv -> v
      v.lowerBounds = tv.lowerBounds.mapConserve(freshen)
      v.upperBounds = tv.upperBounds.mapConserve(freshen)
      v.uni = tv.uni.map(freshenUnification)
      v
    }


    freshen(ty)
  }

  def err(msg: Message -> Opt[Loc])(implicit raise: Raise): SimpleType = {
    raise(ErrorReport(msg :: Nil))
    TypeRef(TypeName("err"), Nil)(noProv)
  }

  def err(msgs: List[Message -> Opt[Loc]])(implicit raise: Raise): SimpleType = {
    raise(ErrorReport(msgs))
    TypeRef(TypeName("err"), Nil)(noProv)
  }

  def warn(msg: Message, loco: Opt[Loc])(implicit raise: Raise): Unit = warn(msg -> loco :: Nil)
  def warn(msgs: List[Message -> Opt[Loc]])(implicit raise: Raise): Unit = raise(WarningReport(msgs))

}
