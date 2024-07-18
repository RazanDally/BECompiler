//package hmloc
//
//import hmloc.utils._
//import hmloc.utils.shorthands._
//import hmloc.Message.MessageContext
//
//import scala.collection.immutable.Queue
//import scala.collection.mutable
//import scala.collection.mutable.{Map => MutMap, Queue => MutQueue, Set => MutSet}
//
//object UnificationHelper extends TyperDatatypes {
//  self: Typer =>
//  def subUnify(args1: Seq[ST], args2: Seq[ST], tr1: ST, tr2: ST, u: Unification): Unit = {
//    val zippedArgs = args1.zip(args2)
//    zippedArgs.foreach { case (arg1, arg2) =>
//      // invoke ctor-uni to compute sub-unifications for their type arguments.
//      val ctor_uni = Ctor_Uni(arg1, arg2, tr1, tr2, u)
//      val unificationQueue = Queue(ctor_uni)
//      val unification = Unification(unificationQueue)
//      enqueueUnification(unification) // enqueue the unification operation
//    }
//  }
//
//  def extrudeDF(df: DataFlow)(implicit lvl: Int): Unit = df match {
//    case c@Constraint(a, b) => extrudeTy(a); extrudeTy(b)
//    case Ctor_Uni(a, b, ctora, ctorb, uni) =>
//      extrudeTy(a); extrudeTy(b); extrudeTy(ctora); extrudeTy(ctorb); extrudeUni(uni)
//  }
//
//  def extrudeUni(uni: Unification)(implicit lvl: Int): Unit = uni.flow.foreach(extrudeDF)
//
//  def extrudeTy(ty: ST)(implicit lvl: Int): Unit = {
//    if (ty.level <= lvl) ty else ty match {
//      case t @ FunctionType(l, r) => extrudeTy(l); extrudeTy(r)
//      case t @ ComposedType(p, l, r) => extrudeTy(l); extrudeTy(r)
//      case t @ TupleType(fs) => fs.foreach(tup => extrudeTy(tup._2))
//      case tv: TypeVariable =>
//        tv.level = lvl
//        tv.uni.foreach(extrudeUni(_))
//      case e @ ExtrType(_) => e
//      case p @ ProvType(und) => extrudeTy(und)
//      case _: RigidTypeVariable => ty
//      case tr @ TypeRef(d, ts) => ts.foreach(extrudeTy)
//    }
//  }
//
//  case class Unification(flow: Queue[DataFlow]) extends Ordered[Unification] {
//    lazy val a: ST = flow.head.getStart
//    lazy val b: ST = flow.last.getEnd
//    lazy val level: Int = flow.iterator.map(_.level).max
//    lazy val sequenceTVs: Set[TV] = {
//      val tvSet: MutSet[TV] = MutSet()
//      constraintSequence.map { case (Constraint(a, b), _) =>
//        a.unwrapProvs match { case tv: TypeVariable => tvSet += tv case _ => ()}
//        b.unwrapProvs match { case tv: TypeVariable => tvSet += tv case _ => ()}
//      }
//      tvSet.toSet
//    }
//
//    override def compare(that: Unification): Int = {
//      val levelComp = this.level.compare(that.level)
//      if (levelComp != 0) {
//        return levelComp
//      }
//      val lengthComp = this.constraintSequence.length.compare(that.constraintSequence.length)
//      if (lengthComp != 0) {
//        return lengthComp
//      }
//      val aComp = a.ord.compare(a, that.a)
//      if (aComp == 0) {
//        return b.ord.compare(b, that.b)
//      }
//      aComp
//    }
//
//    override def toString: Str = s"L: $level [${a.unwrapProvs} ~ ${b.unwrapProvs}, ${flow.mkString(", ")}]"
//
//    def createCtx(implicit ctx: Ctx): ShowCtx =
//      ShowCtx.mk(sequenceTVs.map(_.expOcamlTy()(ctx, sequenceTVs)), "?")
//
//    lazy val constraintSequence: Ls[(Constraint, Int)] = flow.iterator.flatMap {
//      case c: Constraint => (c, level) :: Nil
//      case Ctor_Uni(a, b, ctora, ctorb, uni) =>
//        (Constraint.startTransition(a, ctora), level) :: uni.constraintSequence ::: (Constraint.endTransition(ctorb, b), level) :: Nil
//    }.toList
//
//    def createSequenceString(implicit ctx: Ctx): Str = {
//      implicit val showTV: Set[TV] = sequenceTVs
//
//      // Currently only show type sequence for types at current level of nesting
//      val sequenceMessage: Ls[Message] = constraintSequence.iterator.zipWithIndex.filter(_._1._2 == level).map{
//        case ((c@Constraint(a, b), _), idx) => c.transition match {
//          case Some(true) => msg"(${a.expOcamlTy()(ctx, showTV)}) ~~~~ "
//          case Some(false) => if (idx == constraintSequence.length - 1) msg"(${b.expOcamlTy()(ctx, showTV)})" else msg""
//          case None =>
//            val arrowStr = if (c.dir) "--->" else "<---"
//            val last = idx == constraintSequence.length - 1
//            msg"(${a.expOcamlTy()(ctx, showTV)}) $arrowStr " + (if (last) msg"(${b.expOcamlTy()(ctx, showTV)})" else msg"")
//        }
//      }.toList
//
//      val sctx = createCtx
//      val sb = new mutable.StringBuilder()
//      sequenceMessage.foreach(msg => sb ++= msg.showIn(sctx))
//      sb.toString()
//    }
//
//    def concat(other: Unification): Unification = {
//      assert(b.unwrapProvs == other.a.unwrapProvs, s"$b != ${other.a}")
//      Unification(flow.enqueueAll(other.flow))
//    }
//
//    def nestingLevel: Int = flow.map {
//      case _: Constraint => 0
//      case ctor: Ctor_Uni => 1 + ctor.uni.nestingLevel
//    }.max
//
//    def rev: Unification = Unification(flow.map(_.rev).reverse)
//
//    def createErrorMessage(level: Int = 0, showCtx: Opt[ShowCtx] = N)(implicit ctx: Ctx, showTV: Set[TV]): UniErrReport = {
//      println(s"UERR REPORT $toString")
//      val sctx = showCtx.getOrElse(createCtx)
//      val mainMsg = msg"Type `${a.expOcamlTy()(ctx, Set())}` does not match `${b.expOcamlTy()(ctx, Set())}`"
//      val seqString = createSequenceString
//      def msg(a: ST): Message = a.unwrapProvs match {
//        case tv: TV => msg"(${tv.expOcamlTy()}) is assumed for"
//        case st => msg"(${st.expOcamlTy()}) comes from"
//      }
//
//      def constraintToMessage(c: Constraint, last: Bool = false): Ls[(Message, Ls[Loc], Bool, Int, Bool)] = {
//        val (a, b) = Constraint.unapply(c).get
//        val flow = c.dir
//        val locs = c.getCleanProvs.collect {
//          case TypeProvenance(S(loc), _, _, _) => loc
//        }
//
//        // for the last constraint display both the types
//        // from the sequence of locations show the last one for b
//        if (last) {
//          if (locs.isEmpty) {
//            throw new Exception("No locs for relation")
//          } else if (locs.length == 1) {
//            (msg(a), locs, flow, level, false) :: (msg(b), locs, flow, level, true) :: Nil
//          } else {
//            (msg(a), locs.init, flow, level, false) :: (msg(b), locs.last :: Nil, flow, level, true) :: Nil
//          }
//        } else {
//          (msg(a), locs, flow, level, false) :: Nil
//        }
//      }
//
//      // Helpful show types being projected from their constructors
//      def constructorArgumentMessage(c: Ctor_Uni, leftEnd: Bool, level: Int): (Message, Ls[Loc], Bool, Int, Bool) = {
//        val ty = if (leftEnd) { c.a } else { c.b }
//        val locs = ty.uniqueTypeUseLocations.collect {
//          case TypeProvenance(S(loc), _, _, _) => loc
//        }
//        (msg(ty), locs, false, level, true)
//      }
//
//      val report = {
//        val msgs = flow.iterator.sliding(2).zipWithIndex.collect {
//          // single constraint
//          case (Seq(c: Constraint), _) => constraintToMessage(c, true).map(L(_))
//          // single constructor show projected types
//          case (Seq(ctor@Ctor_Uni(_, _, _, _, uni)), _) =>
//            L(constructorArgumentMessage(ctor, true, level)) ::
//              R(uni.createErrorMessage(level + 1, S(sctx))(ctx, showTV)) ::
//              L(constructorArgumentMessage(ctor, false, level)) ::
//              Nil
//          case (Seq(c: Constraint, _: Constraint), _) => constraintToMessage(c).map(L(_))
//          case (Seq(c: Constraint, _: Ctor_Uni), _) => constraintToMessage(c, true).map(L(_))
//          // if there are two constructors side by side
//          // project their common type once
//          case (Seq(ctor@Ctor_Uni(_, _, _, _, uni), ctor2: Ctor_Uni), idx) if ctor.b === ctor2.a =>
//            val project = L(constructorArgumentMessage(ctor, false, level))
//            val nestedReport = R(uni.createErrorMessage(level + 1, S(sctx))(ctx, showTV))
//            if (idx == 0) {
//              L(constructorArgumentMessage(ctor, true, level)) :: nestedReport :: project :: Nil
//            } else {
//              nestedReport :: project :: Nil
//            }
//          // if constructor is first in the sequence project left type
//          case (Seq(ctor@Ctor_Uni(_, _, _, _, uni), _), idx) =>
//            val nestedReport = R(uni.createErrorMessage(level + 1, S(sctx))(ctx, showTV)) :: Nil
//            if (idx == 0) {
//              L(constructorArgumentMessage(ctor, true, level)) :: nestedReport
//            } else {
//              nestedReport
//            }
//        }.flatten.toList ::: (if (flow.length != 1) flow.last match {
//          case c: Constraint => constraintToMessage(c, true).map(L(_))
//          case ctor@Ctor_Uni(_, _, _, _, uni) =>
//            // if constructor is last in the sequence project type
//            R(uni.createErrorMessage(level + 1, S(sctx))(ctx, showTV)) :: L(constructorArgumentMessage(ctor, false, level)) :: Nil
//        } else { Nil })
//        UniErrReport(mainMsg, seqString, msgs, sctx, level)
//      }
//      report
//    }
//  }
//
//  object Unification {
//    def fromLhsRhs(lhs: ST, rhs: ST): Unification = Unification(Queue(Constraint(lhs, rhs)))
//  }
//
//  sealed abstract class DataFlow {
//    def getStart: ST = this match {
//      case c: Constraint => c.a
//      case c: Ctor_Uni => c.a
//    }
//
//    def getEnd: ST = this match {
//      case c: Constraint => c.b
//      case c: Ctor_Uni => c.b
//    }
//
//    def rev: DataFlow = this match {
//      case c@Constraint(a, b) =>
//        val c1 = Constraint(b, a)
//        c1.dir = !c.dir
//        c1
//      case Ctor_Uni(a, b, ctora, ctorb, flow) =>
//        Ctor_Uni(b, a, ctorb, ctora, flow.rev)
//    }
//
//    lazy val level: Int = this match {
//      case _: Constraint => 0
//      case ctor: Ctor_Uni => ctor.uni.level + 1
//    }
//
//    override def toString: Str = this match {
//      case c@Constraint(a, b) => if (c.dir) s"${a.unwrapProvs} <: ${b.unwrapProvs}" else s"${a.unwrapProvs} :> ${b.unwrapProvs}"
//      case Ctor_Uni(a, b, ctora, ctorb, flow) => s"[${a.unwrapProvs} - ${ctora.unwrapProvs} ~ ${ctorb.unwrapProvs} - ${b.unwrapProvs}, $flow]"
//    }
//  }
//
//  case class Constraint(a: ST, b: ST) extends DataFlow {
//    // true flow from a to b
//    var dir = true
//    // this is a special constrain that shows a transition between levels
//    // this variable is only used during error reporting and not during
//    // actual unification
//    // N - default no transition
//    // S(true) - start transition, `a` goes into `b`
//    // S(false) - end transition, `b` comes out of `a`
//    var transition: Opt[Bool] = N
//    def getCleanProvs: Ls[TP] = {
//      val provs = a.uniqueTypeUseLocations reverse_::: b.uniqueTypeUseLocations
//      if (dir) {
//        // first location binds tighter so only use second prov if it's not same as first
//        provs match {
//          case head :: _ => head :: provs.sliding(2).collect {
//            case Seq(TypeProvenance(S(loc1), _, _, _), tp@TypeProvenance(S(loc2), _, _, _)) if loc1 != loc2 => tp
//          }.toList
//          case _ => provs
//        }
//      } else {
//        // second location binds tighter
//        provs match {
//          case ::(head, _) => head :: provs.sliding(2).collect {
//            case Seq(TypeProvenance(S(loc1), _, _, _), tp@TypeProvenance(S(loc2), _, _, _)) if loc1 != loc2 => tp
//          }.toList
//          case Nil => Nil
//        }
//      }
//    }
//  }
//
//  object Constraint {
//    def startTransition(a: ST, b: ST) = {
//      val c = Constraint(a, b)
//      c.transition = S(true)
//      c
//    }
//    def endTransition(a: ST, b: ST) = {
//      val c = Constraint(a, b)
//      c.transition = S(false)
//      c
//    }
//  }
//  case class Ctor_Uni(a: ST, b: ST, ctora: ST, ctorb: ST, uni: Unification) extends DataFlow
//
//}
