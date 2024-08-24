package hmloc

import hmloc.utils._
import hmloc.utils.shorthands._


trait DataFlowTrait extends TyperDatatypes {
  self: Typer =>
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
    def startTransition(a: ST, b: ST): Constraint = {
      val c = Constraint(a, b)
      c.transition = S(true)
      c
    }
    def endTransition(a: ST, b: ST): Constraint = {
      val c = Constraint(a, b)
      c.transition = S(false)
      c
    }
  }
  case class Ctor_Uni(a: ST, b: ST, ctora: ST, ctorb: ST, uni: Unification) extends DataFlow


}
