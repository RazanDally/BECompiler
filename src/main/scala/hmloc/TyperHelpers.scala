package hmloc

import hmloc.utils._
import hmloc.utils.shorthands._

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet
import scala.collection.mutable.{Map => MutMap, Set => MutSet}

abstract class TyperHelpers { Typer: Typer =>
  
  // Statistics
  protected var constrainCalls, annoyingCalls, subtypingCalls, constructedTypes = 0
  def stats: (Int, Int, Int, Int) = (constrainCalls, annoyingCalls, subtypingCalls, constructedTypes)
  def resetStats(): Unit = { constrainCalls = 0; annoyingCalls = 0; subtypingCalls = 0; constructedTypes = 0 }
  
  // Tracing and debugging
  protected var indent = 0
  private val noPostTrace: Any => String = _ => ""
  def trace[T](pre: => String)(thunk: => T)(post: T => String = noPostTrace): T = {
    println(pre)
    indent += 1
    val res = try thunk finally indent -= 1
    if (post isnt noPostTrace) println(post(res))
    res
  }
  def emitDbg(str: String): Unit = scala.Predef.println(str)
  def println(msg: => Any): Unit = if (dbg) emitDbg("| " * indent + msg)

  // Type operations
  def recordUnion(fs1: Ls[Var -> ST], fs2: Ls[Var -> ST]): Ls[Var -> ST] = {
    val fs2m = fs2.toMap
    fs1.flatMap { case (k, v) => fs2m.get(k).map(v2 => k -> (v | v2)) }
  }

  def subst(ts: PolymorphicType, map: Map[SimpleType, SimpleType]): PolymorphicType =
    PolymorphicType(ts.level, subst(ts.body, map))

  def subst(st: SimpleType, map: Map[SimpleType, SimpleType], substInMap: Bool = false)
        (implicit cache: MutMap[TypeVariable, SimpleType] = MutMap.empty): SimpleType = {
    map.get(st) match {
      case S(res) => if (substInMap) subst(res, map, substInMap) else res
      case N => st match {
        case tv: TypeVariable if tv.lowerBounds.isEmpty && tv.upperBounds.isEmpty =>
          cache += tv -> tv
          tv
        case tv: TypeVariable => cache.getOrElseUpdate(tv, {
          val v = freshVar(tv.prov, tv.nameHint)(tv.level)
          v.lowerBounds = tv.lowerBounds.map(subst(_, map, substInMap))
          v.upperBounds = tv.upperBounds.map(subst(_, map, substInMap))
          v
        })
        case _ => st.map(subst(_, map, substInMap))
      }
    }
  }

  def tupleIntersection(fs1: Ls[Opt[Var] -> ST], fs2: Ls[Opt[Var] -> ST]): Ls[Opt[Var] -> ST] = {
    require(fs1.size === fs2.size)
    (fs1 lazyZip fs2).map {
      case ((S(n1), t1), (S(n2), t2)) if n1 =/= n2 => (N, t1 & t2)
      case ((no1, t1), (no2, t2)) => (no1 orElse no2, t1 & t2)
    }
  }

  def tupleUnion(fs1: Ls[Opt[Var] -> ST], fs2: Ls[Opt[Var] -> ST]): Ls[Opt[Var] -> ST] = {
    require(fs1.size === fs2.size)
    (fs1 lazyZip fs2).map {
      case ((S(n1), t1), (S(n2), t2)) => (Option.when(n1 === n2)(n1), t1 | t2)
      case ((no1, t1), (no2, t2)) => (N, t1 | t2)
    }
  }

  // SimpleType implementation
  trait SimpleTypeImpl { self: SimpleType =>
    implicit val ord: Ordering[ST] = Ordering.by(_.toString)
    
    def showProvOver(enabled: Bool)(str: Str): Str =
      if (enabled) str + prov.toString else str

    override lazy val hashCode: Int = this match {
      case tv: TypeVariable => tv.uid
      case ProvType(und) => und.hashCode
      case p: Product => scala.runtime.ScalaRunTime._hashCode(p)
    }

    override def equals(that: Any): Bool = this match {
      case ProvType(und) => (und: Any) === that
      case tv1: TV => that match {
        case tv2: Typer#TV => tv1.uid === tv2.uid
        case ProvType(und) => this === und
        case _ => false
      }
      case p1: Product => that match {
        case that: ST => that match {
          case ProvType(und) => this === und
          case tv: TV => false
          case p2: Product =>
            p1.canEqual(p2) && p1.productArity === p2.productArity && {
              val it1 = p1.productIterator
              val it2 = p2.productIterator
              while (it1.hasNext && it2.hasNext) {
                if (it1.next() =/= it2.next()) return false
              }
              !it1.hasNext && !it2.hasNext
            }
        }
        case _ => false
      }
    }

    def map(f: SimpleType => SimpleType): SimpleType = this match {
      case FunctionType(lhs, rhs) => FunctionType(f(lhs), f(rhs))(prov)
      case TupleType(fields) => TupleType(fields.mapValues(f(_)))(prov)
      case ComposedType(pol, lhs, rhs) => ComposedType(pol, f(lhs), f(rhs))(prov)
      case ProvType(underlying) => ProvType(f(underlying))(prov)
      case TypeRef(defn, targs) => TypeRef(defn, targs.map(f(_)))(prov)
      case _: TypeVariable | _: RigidTypeVariable | _: ExtrType => this
    }

    def |(that: SimpleType, prov: TypeProvenance = noProv, swapped: Bool = false): SimpleType = (this, that) match {
      case (TopType, _) => TopType
      case (BotType, _) => that
      case (t0@TupleType(fs0), t1@TupleType(fs1)) if fs0.sizeCompare(fs1) === 0 =>
        TupleType(tupleUnion(fs0, fs1))(t0.prov)
      case _ if !swapped => that | (this, prov, swapped = true)
      case (`that`, _) => this
      case _ => ComposedType(true, that, this)(prov)
    }

    def &(that: SimpleType, prov: TypeProvenance = noProv, swapped: Bool = false): SimpleType = (this, that) match {
      case (BotType, _) => BotType
      case (FunctionType(l1, r1), FunctionType(l2, r2)) => FunctionType(l1 | l2, r1 & r2)(prov)
      case (t0@TupleType(fs0), t1@TupleType(fs1)) =>
        if (fs0.sizeCompare(fs1) =/= 0) BotType
        else TupleType(tupleIntersection(fs0, fs1))(t0.prov)
      case _ if !swapped => that & (this, prov, swapped = true)
      case (`that`, _) => this
      case _ => ComposedType(false, that, this)(prov)
    }

    def withProv(p: TypeProvenance): ST = mkProv(this, p)
    def unwrapProvs: SimpleType = this match {
      case ProvType(und) => und.unwrapProvs
      case _ => this
    }

    def children(includeBounds: Bool): List[SimpleType] = this match {
      case tv: TypeVariable => if (includeBounds) tv.uniConcreteTypes.toList else Nil
      case FunctionType(l, r) => l :: r :: Nil
      case ComposedType(_, l, r) => l :: r :: Nil
      case TupleType(fs) => fs.flatMap(f => f._2 :: Nil)
      case ExtrType(_) => Nil
      case ProvType(und) => und :: Nil
      case _: RigidTypeVariable => Nil
      case TypeRef(d, ts) => ts
    }

    def getVars: SortedSet[TypeVariable] = {
      val res = MutSet.empty[TypeVariable]
      @tailrec def rec(queue: List[SimpleType]): Unit = queue match {
        case (tv: TypeVariable) :: tys =>
          if (res(tv)) rec(tys)
          else { res += tv; rec(tv.children(includeBounds = true) ::: tys) }
        case ty :: tys => rec(ty.children(includeBounds = true) ::: tys)
        case Nil => ()
      }
      rec(this :: Nil)
      SortedSet.from(res)(Ordering.by(_.uid))
    }

    def showUnified: String =
      getVars.iterator.filter(tv => tv.uniConcreteTypes.nonEmpty)
        .map(tv => "\n\t\t" + tv.toString + " = " + tv.uniConcreteTypes.mkString(", ")).mkString

    def expOcamlTy()(implicit ctx: Ctx, showTV: Set[TV]): Type =
      expandUnifiedType(this, stopAtTyVars = true, showTV, true)

    def typeUseLocations: Ls[TypeProvenance] = this match {
      case pv: ProvType => pv.prov.loco match {
        case None => pv.underlying.typeUseLocations
        case Some(_) => pv.prov :: pv.underlying.typeUseLocations
      }
      case st => st.prov.loco match {
        case None => Nil
        case Some(_) => st.prov :: Nil
      }
    }

    def uniqueTypeUseLocations: Ls[TypeProvenance] = {
      val stUseLocation = this.typeUseLocations
      stUseLocation.headOption.map(head => head :: stUseLocation.sliding(2).collect {
        case Seq(TypeProvenance(loc1, _, _, _), t@TypeProvenance(loc2, _, _, _)) if loc1 =/= loc2 => t
      }.toList).getOrElse(Nil)
    }
  }
}