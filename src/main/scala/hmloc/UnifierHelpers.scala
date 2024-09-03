package hmloc

import hmloc.Message.MessageContext
import hmloc.utils._
import hmloc.utils.shorthands._

import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.collection.mutable.{Queue => MutQueue, Set => MutSet, Map => MutMap}

trait UnifierHelpers extends TyperDatatypes with DataFlowTrait {
  self : Typer =>
  class UState(
                cache: MutSet[(ST, ST, Int)] = MutSet(),
                val queue: MutQueue[Unification] = MutQueue(),
                val error: MutSet[Unification] = MutSet(),
                // max queue length, total unifications solved
              ) extends Iterator[Unification] {
    var stats: (Int, Int) = (0, 0)
    def enqueueUnification(u: Unification): Unit = if (!cached(u)) {
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
        // there is no if clause for function types as the sub unifier should handle this
        // by checking arguments / return values with eachother


        // TypeVariable already unified
        case (tv1: TypeVariable, tv2: TypeVariable) if tv1 === tv2 => ()

        // U-Var-R
        case (tv: TypeVariable, rhs) =>

          handsideTypeLevel(rhs, tv)

          tv.uni.foreach(tvuni => {
            if (tvuni.a.unwrapProvs == tv) {
              enqueueUnification(tvuni.rev.concat(u))
            } else {
              enqueueUnification(tvuni.concat(u))
            }
          })

          HandleHSTypeVariable(rhs, u)

          tv.uni ::= u

        // U-Var-L
        case (lhs, tv: TypeVariable) =>
          handsideTypeLevel(lhs, tv)

          tv.uni.foreach(tvuni => {
            if (tvuni.a.unwrapProvs == tv) {
              enqueueUnification(u.concat(tvuni))
            } else {
              enqueueUnification(u.concat(tvuni.rev))
            }
          })

          HandleHSTypeVariable(lhs, u)

          // Add the current unification to tv's unification list
          tv.uni ::= u

        case (st1, st2) if st1 != st2 => addError(u)
        case _ => ()
      }

    }

    private def handsideTypeLevel(hs :SimpleType, tv: TypeVariable): Unit = {
      if (hs.level > tv.level) {
        // Extrude the type of lhs to the level of tv
        getType(hs)(tv.level)
        println(s"U EXTR ~> ${hs.toString}")
      }
    }


    /**
     * If hs is a type variable, update its unification list
     * @param hs SimpleType
     * @param u Unification
     */
    private def HandleHSTypeVariable(hs: SimpleType, u: Unification): Unit = {
      hs match {
        case tv: TypeVariable => tv.uni ::= u
        case _ => ()
      }
    }

    private def subUnify(args1: Seq[ST], args2: Seq[ST], tr1: ST, tr2: ST, u: Unification): Unit = {
      val zippedArgs = args1.zip(args2)
      zippedArgs.foreach { case (arg1, arg2) =>
        // invoke ctor-uni to compute sub-unifications for their type arguments.
        val ctor_uni = Ctor_Uni(arg1, arg2, tr1, tr2, u)
        val unificationQueue = Queue(ctor_uni)
        val unification = Unification(unificationQueue)
        enqueueUnification(unification) // enqueue the unification operation
      }
    }

    private def getDataFlow(df: DataFlow)(implicit lvl: Int): Unit = df match {
      case Constraint(a, b) => getType(a); getType(b)
      case Ctor_Uni(a, b, ctora, ctorb, uni) =>
        getType(a); getType(b); getType(ctora); getType(ctorb); getUnification(uni)
    }

    private def getUnification(uni: Unification)(implicit lvl: Int): Unit = uni.flow.foreach(getDataFlow)

    def getType(ty: ST)(implicit lvl: Int): Unit = {
      if (ty.level <= lvl) () else ty match {
        case t @ FunctionType(l, r) => getType(l); getType(r)
        case t @ ComposedType(p, l, r) => getType(l); getType(r)
        case t @ TupleType(fs) => fs.foreach(tup => getType(tup._2))
        case tv: TypeVariable =>
          tv.level = lvl
          tv.uni.foreach(getUnification(_))
        case e @ ExtrType(_) => ()
        case p @ ProvType(und) => getType(und)
        case _: RigidTypeVariable => ()
        case tr @ TypeRef(d, ts) => ts.foreach(getType)
      }
    }

    def subsume(ty_sch: PolymorphicType, sign: PolymorphicType)
               (implicit ctx: Ctx, raise: Raise, prov: TypeProvenance): Unit = {
      enqueueUnification(Unification.fromLhsRhs(ty_sch.instantiate, sign.rigidify))
    }
  }

}
