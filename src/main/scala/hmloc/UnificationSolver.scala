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
trait UnificationSolver extends TyperDatatypes {
  self: Typer =>

  val cache: MutSet[(ST, ST)] = MutSet()
  var unifyMode: Bool = false

  class UnificationState (
    cache: MutSet[(ST, ST, Int)] = MutSet(),
    val queue: MutQueue[Unification] = MutQueue(),
    val error: MutSet[Unification] = MutSet(),
    // max queue length, total unifications solved
  ) extends Iterator[Unification] {
    var stats: (Int, Int) = (0, 0)
    def enqueueUnification(u: Unification): Unit = if (!cached(u)) {
      // TODO: fix this so that recursion can be stopped
      if (u.level >= 4) {
        println(s"U X ${u.a} ~ ${u.b}")
        return
      }
      println(s"U Q ${u.a} ~ ${u.b}")
      queue += u
      cache(u)
      stats = (math.max(stats._1, queue.length), stats._2 + 1)
    }
    def cached(u: Unification): Bool =
      cache((u.a.unwrapProvs, u.b.unwrapProvs, u.level)) || cache((u.b.unwrapProvs, u.a.unwrapProvs, u.level))
    def cache(u: Unification): Unit = cache += ((u.a, u.b, u.level))
    def addError(u: Unification): Unit = {
      println(s"UERR $u")
      error += u
    }
    def clear(): Unit = {
      cache.clear()
      queue.clear()
      error.clear()
      stats = (0, 0)
    }

    def reportStats: Str = s"U max: ${stats._1}, total: ${stats._2}"
    override def hasNext: Bool = queue.nonEmpty
    override def next(): Unification = queue.dequeue()

    def unify(a: ST, b: ST): Unit = {
      enqueueUnification(Unification.fromLhsRhs(a, b))
      unify()
    }

    def unify(): Unit = foreach { u =>
      println(s"U $u")
      val st1 = u.a.unwrapProvs
      val st2 = u.b.unwrapProvs

      (st1, st2) match {

        // U-Sub for typeRef
        case (tr1: TypeRef, tr2: TypeRef) if tr1.defn === tr2.defn && tr1.targs.length === tr2.targs.length =>
          subUnify(tr1.targs, tr2.targs, st1, st2, u)

        // U-Error for typeRef
        case (_: TypeRef, _: TypeRef) => addError(u)

        // U-Sub for tuple
        case (tup1: TupleType, tup2: TupleType) if tup1.fields.length === tup2.fields.length =>
          subUnify(tup1.fields.map(_._2), tup2.fields.map(_._2), tup1, tup2, u)

        // U-Error for Tuple
        case (_: TupleType, _: TupleType) => addError(u)

        // U-Sub for FunctionType
        case (FunctionType(arg1, res1), FunctionType(arg2, res2)) =>
          subUnify(Seq(arg1), Seq(arg2), st1, st2, u)
          subUnify(Seq(res1), Seq(res2), st1, st2, u)

        // U-Error for FunctionType
        case (_: FunctionType, _: FunctionType) => addError(u)

        // TypeVariable already unified
        case (tv1: TypeVariable, tv2: TypeVariable) if tv1 === tv2 => ()

        // U-Var-R
        case (tv: TypeVariable, rhs) =>

          if (rhs.level > tv.level) {
            // Extrude the type of rhs to the level of tv
            extrudeTy(rhs)(tv.level)
            println(s"U EXTR ~> ${rhs.toString}")
          }

          tv.uni.foreach(tvuni => {
            if (tvuni.a.unwrapProvs == tv) {
              enqueueUnification(tvuni.rev.concat(u))
            } else {
              enqueueUnification(tvuni.concat(u))
            }
          })

          rhs match {
            case rhsTv: TypeVariable => rhsTv.uni ::= u
            case _ => ()
          }

          tv.uni ::= u

        // U-Var-L
        case (lhs, tv: TypeVariable) =>
          if (lhs.level > tv.level) {
            // Extrude the type of lhs to the level of tv
            extrudeTy(lhs)(tv.level)
            println(s"U EXTR ~> ${lhs.toString}")
          }

          tv.uni.foreach(tvuni => {
            if (tvuni.a.unwrapProvs == tv) {
              enqueueUnification(u.concat(tvuni))
            } else {
              enqueueUnification(u.concat(tvuni.rev))
            }
          })

          // If lhs is a type variable, update its unification list
          lhs match {
            case lhsTv: TypeVariable => lhsTv.uni ::= u
            case _ => ()
          }

          // Add the current unification to tv's unification list
          tv.uni ::= u
        case (st1, st2) if st1 != st2 => addError(u)
        case _ => ()
      }

    }

    def subUnify(args1: Seq[ST], args2: Seq[ST], tr1: ST, tr2: ST, u: Unification): Unit = {
      val zippedArgs = args1.zip(args2)
      zippedArgs.foreach { case (arg1, arg2) =>
        // invoke ctor-uni to compute sub-unifications for their type arguments.
        val ctor_uni = Ctor_Uni(arg1, arg2, tr1, tr2, u)
        val unificationQueue = Queue(ctor_uni)
        val unification = Unification(unificationQueue)
        enqueueUnification(unification) // enqueue the unification operation
      }
    }

    def extrudeDF(df: DataFlow)(implicit lvl: Int): Unit = df match {
      case c@Constraint(a, b) => extrudeTy(a); extrudeTy(b)
      case Ctor_Uni(a, b, ctora, ctorb, uni) =>
        extrudeTy(a); extrudeTy(b); extrudeTy(ctora); extrudeTy(ctorb); extrudeUni(uni)
    }

    def extrudeUni(uni: Unification)(implicit lvl: Int): Unit = uni.flow.foreach(extrudeDF)

    def extrudeTy(ty: ST)(implicit lvl: Int): Unit = {
      if (ty.level <= lvl) ty else ty match {
        case t @ FunctionType(l, r) => extrudeTy(l); extrudeTy(r)
        case t @ ComposedType(p, l, r) => extrudeTy(l); extrudeTy(r)
        case t @ TupleType(fs) => fs.foreach(tup => extrudeTy(tup._2))
        case tv: TypeVariable =>
          tv.level = lvl
          tv.uni.foreach(extrudeUni(_))
        case e @ ExtrType(_) => e
        case p @ ProvType(und) => extrudeTy(und)
        case _: RigidTypeVariable => ty
        case tr @ TypeRef(d, ts) => ts.foreach(extrudeTy)
      }
    }

    def subsume(ty_sch: PolymorphicType, sign: PolymorphicType)
               (implicit ctx: Ctx, raise: Raise, prov: TypeProvenance): Unit = {
      enqueueUnification(Unification.fromLhsRhs(ty_sch.instantiate, sign.rigidify))
    }
  }

  val uniState = new UnificationState()

  def outputUnificationErrors(): Ls[Str] = {
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

    def createCtx(implicit ctx: Ctx): ShowCtx =
      ShowCtx.mk(sequenceTVs.map(_.expOcamlTy()(ctx, sequenceTVs)), "?")

    lazy val constraintSequence: Ls[(Constraint, Int)] = flow.iterator.flatMap {
      case c: Constraint => (c, level) :: Nil
      case Ctor_Uni(a, b, ctora, ctorb, uni) =>
        (Constraint.startTransition(a, ctora), level) :: uni.constraintSequence ::: (Constraint.endTransition(ctorb, b), level) :: Nil
    }.toList

    def createSequenceString(implicit ctx: Ctx): Str = {
      implicit val showTV: Set[TV] = sequenceTVs

      // Currently only show type sequence for types at current level of nesting
      val sequenceMessage: Ls[Message] = constraintSequence.iterator.zipWithIndex.filter(_._1._2 == level).map{
        case ((c@Constraint(a, b), _), idx) => c.transition match {
          case Some(true) => msg"(${a.expOcamlTy()(ctx, showTV)}) ~~~~ "
          case Some(false) => if (idx == constraintSequence.length - 1) msg"(${b.expOcamlTy()(ctx, showTV)})" else msg""
          case None =>
            val arrowStr = if (c.dir) "--->" else "<---"
            val last = idx == constraintSequence.length - 1
            msg"(${a.expOcamlTy()(ctx, showTV)}) $arrowStr " + (if (last) msg"(${b.expOcamlTy()(ctx, showTV)})" else msg"")
        }
      }.toList

      val sctx = createCtx
      val sb = new mutable.StringBuilder()
      sequenceMessage.foreach(msg => sb ++= msg.showIn(sctx))
      sb.toString()
    }

    def concat(other: Unification): Unification = {
      assert(b.unwrapProvs == other.a.unwrapProvs, s"$b != ${other.a}")
      Unification(flow.enqueueAll(other.flow))
    }

    def nestingLevel: Int = flow.map {
      case _: Constraint => 0
      case ctor: Ctor_Uni => 1 + ctor.uni.nestingLevel
    }.max

    def rev: Unification = Unification(flow.map(_.rev).reverse)

    def createErrorMessage(level: Int = 0, showCtx: Opt[ShowCtx] = N)(implicit ctx: Ctx, showTV: Set[TV]): UniErrReport = {
      println(s"UERR REPORT $toString")
      val sctx = showCtx.getOrElse(createCtx)
      val mainMsg = msg"Type `${a.expOcamlTy()(ctx, Set())}` does not match `${b.expOcamlTy()(ctx, Set())}`"
      val seqString = createSequenceString
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

  sealed abstract class DataFlow {
    def getStart: ST = this match {
      case c: Constraint => c.a
      case c: Ctor_Uni => c.a
    }

    def getEnd: ST = this match {
      case c: Constraint => c.b
      case c: Ctor_Uni => c.b
    }

    def rev: DataFlow = this match {
      case c@Constraint(a, b) =>
        val c1 = Constraint(b, a)
        c1.dir = !c.dir
        c1
      case Ctor_Uni(a, b, ctora, ctorb, flow) =>
        Ctor_Uni(b, a, ctorb, ctora, flow.rev)
    }

    lazy val level: Int = this match {
      case _: Constraint => 0
      case ctor: Ctor_Uni => ctor.uni.level + 1
    }

    override def toString: Str = this match {
      case c@Constraint(a, b) => if (c.dir) s"${a.unwrapProvs} <: ${b.unwrapProvs}" else s"${a.unwrapProvs} :> ${b.unwrapProvs}"
      case Ctor_Uni(a, b, ctora, ctorb, flow) => s"[${a.unwrapProvs} - ${ctora.unwrapProvs} ~ ${ctorb.unwrapProvs} - ${b.unwrapProvs}, $flow]"
    }
  }

  case class Constraint(a: ST, b: ST) extends DataFlow {
    // true flow from a to b
    var dir = true
    // this is a special constrain that shows a transition between levels
    // this variable is only used during error reporting and not during
    // actual unification
    // N - default no transition
    // S(true) - start transition, `a` goes into `b`
    // S(false) - end transition, `b` comes out of `a`
    var transition: Opt[Bool] = N
    def getCleanProvs: Ls[TP] = {
      val provs = a.uniqueTypeUseLocations reverse_::: b.uniqueTypeUseLocations
      if (dir) {
        // first location binds tighter so only use second prov if it's not same as first
        provs match {
          case head :: _ => head :: provs.sliding(2).collect {
            case Seq(TypeProvenance(S(loc1), _, _, _), tp@TypeProvenance(S(loc2), _, _, _)) if loc1 != loc2 => tp
          }.toList
          case _ => provs
        }
      } else {
        // second location binds tighter
        provs match {
          case ::(head, _) => head :: provs.sliding(2).collect {
            case Seq(TypeProvenance(S(loc1), _, _, _), tp@TypeProvenance(S(loc2), _, _, _)) if loc1 != loc2 => tp
          }.toList
          case Nil => Nil
        }
      }
    }
  }

  object Constraint {
    def startTransition(a: ST, b: ST) = {
      val c = Constraint(a, b)
      c.transition = S(true)
      c
    }
    def endTransition(a: ST, b: ST) = {
      val c = Constraint(a, b)
      c.transition = S(false)
      c
    }
  }
  case class Ctor_Uni(a: ST, b: ST, ctora: ST, ctorb: ST, uni: Unification) extends DataFlow


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
