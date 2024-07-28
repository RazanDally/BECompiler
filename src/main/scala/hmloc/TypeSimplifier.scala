package hmloc

import hmloc.utils._
import hmloc.utils.shorthands._

import scala.collection.mutable.{Map => MutMap}

trait TypeSimplifier { self: Typer =>
  
  abstract class SimplifyPipeline {
    def debugOutput(msg: => Str): Unit
    
    def apply(st: ST)(implicit ctx: Ctx): ST = {
      def unifyTypeVariable(ty: SimpleType)(implicit ctx: Ctx): SimpleType = trace(s"UTY: ${ty}") {
        val tvMap: MutMap[TV, ST] = MutMap.empty

        def process(ty: SimpleType): SimpleType = ty match {
          case tv: TypeVariable => processTypeVariable(tv)
          case t@FunctionType(l, r) => FunctionType(process(l), process(r))(t.prov)
          case t@ComposedType(p, l, r) => ComposedType(p, process(l), process(r))(t.prov)
          case t@TupleType(fs) => TupleType(fs.mapValues(process))(t.prov)
          case e@ExtrType(_) => e
          case p@ProvType(und) => ProvType(process(und))(p.prov)
          case _: RigidTypeVariable => ty
          case tr@TypeRef(d, ts) => TypeRef(d, ts.map(process))(tr.prov)
        }

        def processTypeVariable(tv: TypeVariable): SimpleType = {
          tvMap.getOrElse(tv, {
            val (tvs, sts) = partitionUnifiedTypes(tv)

            debugOutput(s"Unifying: $tv with $tvs and $sts")

            val nv = if (tvs.nonEmpty) createAndMapNewTypeVariable(tv, tvs) else mapToItself(tv)

            nv.uniConcreteTypes = sts.map(process).toSet
            debugOutput(s"Mapped: $sts to ${nv.uniConcreteTypes}")

            nv
          })
        }

        def partitionUnifiedTypes(tv: TypeVariable): (List[TypeVariable], List[SimpleType]) = {
          tv.unifiedWith.partitionMap {
            case tv: TypeVariable => L(tv)
            case st => R(st)
          }
        }

        def createAndMapNewTypeVariable(tv: TypeVariable, tvs: List[TypeVariable]): TypeVariable = {
          val nv = freshVar(tv.prov, tv.nameHint)(tv.level)
          debugOutput(s"Mapped: ${(tv :: tvs).mkString(", ")} to $nv")
          (tv :: tvs).foreach(tvMap.put(_, nv))
          nv
        }

        def mapToItself(tv: TypeVariable): TypeVariable = {
          tvMap.put(tv, tv)
          tv
        }

        process(ty)
      }(res => s"STY: $res")

      // if type variable is unified with only one concrete type replace it with the concrete type
      def simplifyTypeVariable(ty: SimpleType)(implicit ctx: Ctx): SimpleType = {
        val tvMap: MutMap[TV, ST] = MutMap.empty

        def process(ty: SimpleType): SimpleType = trace(s"S: $ty ${ty.getClass.getSimpleName}") {
          ty match {
            case tv: TV => processTypeVariable(tv)
            case t@FunctionType(l, r) => FunctionType(process(l), process(r))(t.prov)
            case t@ComposedType(p, l, r) => ComposedType(p, process(l), process(r))(t.prov)
            case t@TupleType(fs) => TupleType(fs.mapValues(process))(t.prov)
            case e@ExtrType(_) => e
            case p@ProvType(und) => ProvType(process(und))(p.prov)
            case _: RigidTypeVariable => ty
            case tr@TypeRef(d, ts) => TypeRef(d, ts.map(process))(tr.prov)
          }
        }(res => s"S: $res")

        def processTypeVariable(tv: TV): SimpleType = {
          tvMap.getOrElseUpdate(tv, {
            // Temporarily map type to prevent infinite recursion
            tvMap += ((tv, tv))
            tv.uniConcreteTypes = tv.uniConcreteTypes.map(process)

            tv.uniConcreteTypes.toSeq match {
              case Seq(ty) =>
                println(s"Simplified: $tv to $ty")
                ty
              case _ => tv
            }
          })
        }
        process(ty)
      }

      var cur = st
      cur = unifyTypeVariable(st)
      cur = simplifyTypeVariable(cur)
      debugOutput(s"⬤ Unified: ${cur}")
      debugOutput(s" where: ${cur.showUnified}")

      cur
    }
  }
}
