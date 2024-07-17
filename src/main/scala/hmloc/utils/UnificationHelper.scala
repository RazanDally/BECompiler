package hmloc.utils

import hmloc.utils.shorthands._
import hmloc._
import fastparse._
import fastparse.Parsed.Failure
import fastparse.Parsed.Success

import scala.collection.mutable
import scala.collection.mutable.{Map => MutMap}
import scala.collection.mutable.Buffer
import os.Path

import scala.collection.immutable.Queue

object UnificationHelper {
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
}
